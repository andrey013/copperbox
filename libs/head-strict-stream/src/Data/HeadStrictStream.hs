{-# LANGUAGE ExistentialQuantification  #-}
{-# OPTIONS -Wall #-}
{-# OPTIONS_GHC -fenable-rewrite-rules  #-}


--------------------------------------------------------------------------------
-- |
-- Module      :  Data.HeadStrictStream
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Head strict stream
--
--------------------------------------------------------------------------------


module Data.HeadStrictStream
  ( 
  -- * Stream
    Stream

  , cons
  , head
  , tail

  , map
  , intersperse
  , interleave
  , iterate
  , repeat
  , cycle
  , unfold
  , take
  , drop
  , dropWhile
  , filter
  , partition
  , zip
  , zipWith
  , unzip

  , ViewL(..)
  , viewl

  , const
  , branch

  ) where 

import Prelude hiding ( head, tail, map, iterate, repeat, cycle
                      , take, drop, dropWhile, filter, zip, zipWith, unzip
                      , const )

import Data.Monoid
import Text.Show

--------------------------------------------------------------------------------


data Stream a = forall st. Stream (st -> Step a st) st


-- | Note Step is strict on the /element/ of Yield.
--
data Step a st = Yield !a st
               | Skip st

-- | As per the Switch datatype in Stream-Fusion, a counting data type.
--
data Count = C1 | C2 deriving (Eq,Ord)

-- | A strict-pair.
--
data SP a b = SP !a !b


-- | A strict-triple.
--
data SP3 a b c = SP3 !a !b !c



--------------------------------------------------------------------------------
-- instances

instance Functor Stream where fmap = map


instance Show a => Show (Stream a) where
  showsPrec _ s = showListWith id $ body `mappend` [showString "..."] 
    where
      body = take 10 $ map shows s


instance Ord a => Ord (Stream a) where
  compare s1 s2 = go (viewl s1) (viewl s2)
    where 
      go (a :< as) (b :< bs) = case compare a b of
                                 LT -> LT
                                 GT -> GT
                                 EQ -> go (viewl as) (viewl bs)



-- | Note - proving equality might take a very long time...
--
instance Eq a => Eq (Stream a) where
  (==) s1 s2 = go (viewl s1) (viewl s2)
    where
      go (a :< as) (b :< bs) | a /= b = False
                             | otherwise = go (viewl as) (viewl bs) 


  (/=) s1 s2 = go (viewl s1) (viewl s2)
    where
      go (a :< as) (b :< bs) | a /= b    = True
                             | otherwise = go (viewl as) (viewl bs) 
                     


-- | Partial instance for Enum.
--
instance Enum a => Enum (Stream a) where
  toEnum i          = repeat (toEnum i)
  fromEnum _        = error $ "HeadStrictStream.fromEnum - not defined"


instance Num a => Num (Stream a) where
  (+)               = zipWith (+)
  (-)               = zipWith (-) 
  (*)               = zipWith (*)
  abs               = map abs
  negate            = map negate
  signum            = map signum
  fromInteger i     = repeat (fromInteger i)


-- | Dummy instance for Real.
--
instance Real a => Real (Stream a) where
  toRational        =  error "HeadStrictStream.toRational - not defined"


-- | Partial instance for Enum (no toInteger).
--
instance Integral a => Integral (Stream a) where
  div               = zipWith div
  mod               = zipWith mod
  quotRem p q       = unzip (zipWith quotRem p q)
  toInteger _       = error $ "HeadStrictStream.toInteger - not defined"

instance Fractional a => Fractional (Stream a) where
  (/)               = zipWith (/)
  recip             = map recip
  fromRational r    = repeat (fromRational r)

--------------------------------------------------------------------------------

--
-- The pattern in the Stream Fusion library seems to be that 
-- definitions in /where/ clauses get an INLINE pragma if they use
-- pattern matching.
-- 


-- | Append an element to the front of a stream, this is
-- synonymous to @(:)@ for lists.
--
cons :: a -> Stream a -> Stream a 
cons z (Stream phi s0) = Stream chi (SP C1 s0)
  where
    -- chi starts in C1, after first step remains in C2
    {-# INLINE chi #-}
    chi (SP C1 st) = Yield z (SP C2 st)
    chi (SP C2 st) = case phi st of
        Yield a s' -> Yield a (SP C2 s') 
        Skip s'    -> Skip (SP C2 s')

{-# INLINE [0] cons #-}


-- | Extract the first element of a stream.
--
head :: Stream a -> a
head (Stream phi s0) = go (phi s0)
  where
    {-# INLINE go #-}
    go (Skip st)   = go (phi st)
    go (Yield a _) = a

{-# INLINE [0] head #-}


-- | Extract the elements after the head of a stream.
--
tail :: Stream a -> Stream a 
tail (Stream phi s0) = go (phi s0)
  where
    {-# INLINE go #-}
    go (Skip st)    = go (phi st)
    go (Yield _ st) = Stream phi st

{-# INLINE [0] tail #-}


-- | 'map' : @ fn * ss -> Stream@
--
--  Apply the supplied function @fn@ to all elements of the stream @ss@.
--
map :: (a -> b) -> Stream a -> Stream b
map f (Stream phi s0) = Stream chi s0
  where
    chi st  = case phi st of
                Yield a s' -> Yield (f a) s'
                Skip s'    -> Skip s'

{-# INLINE [0] map #-}

--
-- The famous map/ map rule.
--
{-# RULES
    "map/map" forall f g s.
        map f (map g s) = map (\x -> f (g x)) s
 #-}


-- | 'intersperse' : @ a * ss -> Stream @
--
-- Intersperse the supplied element @a@ between all the elements
-- in the stream @ss@.
--
-- > [# s0, a, s1, a, s2, a ... #]
--

intersperse :: a -> Stream a -> Stream a
intersperse z (Stream phi s0) = Stream chi (SP C1 s0)
  where
    {-# INLINE chi #-}
    chi (SP C1 st) = case phi st of
        Yield a s' -> Yield a (SP C2 s')   -- switch to C2
        Skip s'    -> Skip (SP C1 s')      -- remain in C1

    chi (SP C2 st) = Yield z (SP C1 st)

-- Follow the Stream Fusion library, intersperse is not inlined, 
-- but the inner definition (here called chi) is.

-- | 'interleave' : @ sa * sb -> Stream @
--
-- Interleave the streams @sa@ and @sb@ alternate elements from them.
--
-- > [# a0, b0, a1, b1, ... #]
--
interleave :: Stream a -> Stream a -> Stream a
interleave (Stream aphi as0) (Stream bphi bs0) = Stream chi (SP3 C1 as0 bs0)
  where
    {-# INLINE chi #-}
    chi (SP3 C1 ast bst)  = case aphi ast of
        Yield a s' -> Yield a (SP3 C2 s' bst)   -- switch to C2
        Skip s'    -> Skip (SP3 C1 s' bst)      -- remain in C1

    chi (SP3 C2 ast bst)  = case bphi bst of
        Yield a s' -> Yield a (SP3 C1 ast s')   -- switch to C1
        Skip s'    -> Skip (SP3 C2 ast s')      -- remain in C2



-- | 'iterate' : @ fn * x -> Stream @
--
-- Build a stream by repeatedly applying the function fn to the seed @x@.
--
-- > [# x, f x, f (f x), f (f (f x)), ... #]
--
iterate :: (a -> a) -> a -> Stream a
iterate f a = Stream phi a
  where
    phi e = Yield e (f e)

{-# INLINE [0] iterate #-}


-- | 'repeat' : @ x -> Stream @
--
-- Build a stream, repeating the seed @x@.
--
-- > [# x, x, x, x, ... #]
--
repeat :: a -> Stream a
repeat a = Stream phi a 
  where
    phi e = Yield e e

{-# INLINE [0] repeat #-}


-- | Cycle the elements of the inpiut list to form a stream.
-- 
-- Note - 'cycle' throws and error when supplied with the empty
-- list.
--
cycle :: [a] -> Stream a
cycle []  = error "HeadStracitStream.cycle - empty list"
cycle xs0 = Stream phi xs0
  where
    phi (y:ys) = Yield y ys
    phi []     = Skip xs0

{-# INLINE [0] cycle #-}

-- | 'unfold' : @ fn * x -> Stream @
--
-- Build a stream by applying the supplied function @fn@ to the 
-- initial state @x@, then subsequently apply @fn@ to the result 
-- state produced at the previous iteration.
--
unfold :: (st -> (a,st)) -> st -> Stream a
unfold f s0 = Stream phi s0
  where
    phi st = let (a,st') = f st in Yield a st'

{-# INLINE [0] unfold #-}

-- | 'take' : @ len * ss -> List @
-- 
-- Take @len@ items from the stream @ss@ building a list.
--
take :: Int -> Stream a -> [a]
take len (Stream phi s0) = go len (phi s0)
  where
    {-# INLINE go #-}
    go n _            | n < 1 = []
    go n (Yield a s')         = a : go (n-1) (phi s')
    go n (Skip s')            = go n (phi s')

{-# INLINE [0] take #-}

-- | 'drop' : @ len * ss -> Stream @
--
-- Drop a prefix of @len@ items from the stream @ss@.
--
drop :: Int -> Stream a -> Stream a
drop len (Stream phi s0) = Stream chi (SP3 C1 len s0)
  where
    {-# INLINE chi #-}
    chi (SP3 C1 n st) | n < 1 = Skip (SP3 C2 0 st)      -- switch to C2
    chi (SP3 C1 n st)         = case phi st of
        Skip s'    -> Skip (SP3 C1 n s')                -- remain in C1
        Yield _ s' -> Skip (SP3 C1 (n-1) s')            -- remain in C1, drop @a@

    chi (SP3 C2 n st)         = case phi st of
        Skip s'    -> Skip (SP3 C2 n s')                -- remain in C2
        Yield a s' -> Yield a (SP3 C2 n s')             -- remain in C2


-- | 'dropWhile' : @ test * ss -> Stream @
--
-- Drop the elements from the prefix of stream @ss@ while the 
-- elements satisfy the predicate @test@.
--
dropWhile :: (a -> Bool) -> Stream a -> Stream a
dropWhile test (Stream phi s0) = Stream chi (SP C1 s0)
  where
    {-# INLINE chi #-}
    chi (SP C1 st) = case phi st of 
        Skip s'    -> Skip (SP C1 s')
        Yield a s' -> if test a then Skip (SP C1 s') else Yield a (SP C2 s')
     
    chi (SP C2 st) = case phi st of 
        Skip s'    -> Skip (SP C2 s')
        Yield a s' -> Yield a (SP C2 s')
    

-- | 'filter' : @ test * ss -> Stream @
--
-- Return the stream where the elementsof the input stream @ss@ 
-- satisfy the predicate @test@.
--
filter :: (a -> Bool) -> Stream a -> Stream a
filter test (Stream phi s0) = Stream chi s0
  where
    chi st = case phi st of
        Yield a s' -> if test a then Yield a s' else chi s'
        Skip s'    -> chi s'

{-# INLINE filter #-}

-- | 'partition' : @ test * ss -> (Stream, Stream)@
--
-- Partition the input list @ss@ into a pair of streams. Elements 
-- that satisfy the predicate @test@ are placed in the left 
-- stream, elements that fail the test are placed in the right 
-- stream. 
--
partition :: (a -> Bool) -> Stream a -> (Stream a, Stream a)
partition test (Stream phi s0) = (Stream lchi s0, Stream rchi s0)
  where
    lchi st = case phi st of
        Yield a s' -> if test a then Yield a s' else lchi s'
        Skip s'    -> lchi s'

    rchi st = case phi st of
        Yield a s' -> if not (test a) then Yield a s' else rchi s'
        Skip s'    -> rchi s'

-- | 'zip' takes two input streams and forms a single stream by 
-- pairing the elements of the input.
--
zip :: Stream a -> Stream b -> Stream (a,b)
zip (Stream aphi as0) (Stream bphi bs0) = Stream chi (SP as0 bs0)
  where
    {-# INLINE chi #-}
    chi (SP ast bst) = case aphi ast of
        Yield a s' -> goB a s' (bphi bst)
        Skip s'    -> chi (SP s' bst) 

    {-# INLINE goB #-}
    goB a ast (Yield b bst) = Yield (a,b) (SP ast bst)
    goB a ast (Skip bst)    = goB a ast (bphi bst)



-- | 'zipWith' : @ op * ss1 * ss2 -> Stream @
--
-- 'zipWith' generalizes 'zip' by using the applying the 
-- operator @op@ to the elements it consumes from the input 
-- streams @ss1@ and @ss2@ to form the result stream. 
-- 
zipWith :: (a -> b -> c) -> Stream a -> Stream b -> Stream c
zipWith op (Stream aphi as0) (Stream bphi bs0) = Stream chi (SP as0 bs0)
  where
    {-# INLINE chi #-}
    chi (SP ast bst) = case aphi ast of
        Yield a s' -> goB a s' (bphi bst)
        Skip s'    -> chi (SP s' bst) 

    {-# INLINE goB #-}    
    goB a ast (Yield b bst) = Yield (a `op` b) (SP ast bst)
    goB a ast (Skip bst)    = goB a ast (bphi bst)


-- | Trasform a stream of pairs into two streams representing the 
-- lefts and rights of the pairs.
--
unzip :: Stream (a,b) -> (Stream a, Stream b)
unzip (Stream phi s0) = (Stream lchi s0, Stream rchi s0) 
  where
    lchi st = case phi st of
        Yield (a,_) s' -> Yield a s'
        Skip s'        -> lchi s'

    rchi st = case phi st of
        Yield (_,b) s' -> Yield b s'
        Skip s'        -> rchi s'

--------------------------------------------------------------------------------
-- Destructing 

-- | /Left view/ of a stream.
--
-- This is used for pattern matching the head and tail of a 
-- stream as 'Stream' is an opaque type.
--
data ViewL a = !a :< Stream a

-- | Destructure a stream into the head and the tail, wrapped as 
-- a ViewL.
--
viewl :: Stream a -> ViewL a
viewl (Stream phi s0) = go (phi s0)
  where
    {-# INLINE go #-}
    go (Yield a s') = a :< (Stream phi s')
    go (Skip s')    = go (phi s')

{-# INLINE [0] viewl #-}


--------------------------------------------------------------------------------

-- | 'const' a:0:0:...
--
const :: Num a => a -> Stream a
const a = Stream phi a 
  where
    phi e = Yield e 0

-- | Branch a stream alternating the production of elements into 
-- the left and right streams of the result pair.
--
branch :: Stream a -> (Stream a, Stream a)
branch (Stream phi s0) = (Stream chi (SP C1 s0), Stream chi (SP C2 s0))
  where
    {-# INLINE chi #-}
    chi (SP C1 st) = case phi st of 
      Yield a s' -> Yield a (SP C2 s')        -- switch to C2, yield @a@
      Skip s'    -> chi (SP C1 s')            -- do until a yield

    chi (SP C2 st) = case phi st of 
      Yield _ s' -> Skip (SP C1 s')           -- switch to C1, drop @a@
      Skip s'    -> chi (SP C2 s')            -- do until a yield
    