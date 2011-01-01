{-# LANGUAGE ExistentialQuantification  #-}
{-# OPTIONS -Wall #-}

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

  , const


  ) where 

import Prelude ( Functor(..), ($), id, error, Eq(..), Ord(..), Num(..), 
                 Fractional(..),
                 foldr, Int, (<=), (-), (.) )

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

--------------------------------------------------------------------------------


cons :: a -> Stream a -> Stream a 
cons z (Stream phi s0) = Stream chi (SP C1 s0)
  where
    -- chi starts in C1, after first step remains in C2
    chi (SP C1 st) = Yield z (SP C2 st)
    chi (SP C2 st) = case phi st of
                       Yield a st' -> Yield a (SP C2 st') 
                       Skip st'    -> Skip (SP C2 st')


head :: Stream a -> a
head (Stream phi s0) = go (phi s0)
  where
    go (Skip st)   = go (phi st)
    go (Yield a _) = a

tail :: Stream a -> Stream a 
tail (Stream phi s0) = go (phi s0)
  where
    go (Skip s1)    = go (phi s1)
    go (Yield _ s1) = Stream phi s1




map :: (a -> b) -> Stream a -> Stream b
map f (Stream phi s0) = Stream chi s0
  where
    chi st  = case phi st of
                Yield a s1 -> Yield (f a) s1
                Skip s1    -> Skip s1


intersperse :: a -> Stream a -> Stream a
intersperse z (Stream phi s0) = Stream chi (SP C1 s0)
  where
    chi (SP C1 st) = case phi st of
        Yield a s1 -> Yield a (SP C2 s1)   -- switch to C2
        Skip s1    -> Skip (SP C1 s1)      -- remain in C1

    chi (SP C2 st) = Yield z (SP C1 st)


    -- C2 is inter-state - yields a


interleave :: Stream a -> Stream a -> Stream a
interleave (Stream aphi as0) (Stream bphi bs0) = Stream chi (SP3 C1 as0 bs0)
  where
    chi (SP3 C1 ast bst)  = case aphi ast of
        Yield a s1 -> Yield a (SP3 C2 s1 bst)   -- switch to C2
        Skip s1    -> Skip (SP3 C1 s1 bst)      -- remain in C1

    chi (SP3 C2 ast bst)  = case bphi bst of
        Yield a s1 -> Yield a (SP3 C1 ast s1)   -- switch to C1
        Skip s1    -> Skip (SP3 C2 ast s1)      -- remain in C2


iterate :: (a -> a) -> a -> Stream a
iterate f a = Stream phi a
  where
    phi e = Yield e (f e)


repeat :: a -> Stream a
repeat a = Stream phi a 
  where
    phi e = Yield e e


-- | Note - 'cycle' throws and error when supplied with the empty
-- list.
--
cycle :: [a] -> Stream a
cycle []  = error "HeadStracitStream.cycle - empty list"
cycle xs0 = Stream phi xs0
  where
    phi (y:ys) = Yield y ys
    phi []     = Skip xs0


unfold :: (st -> (a,st)) -> st -> Stream a
unfold f st0 = Stream phi st0
  where
    phi st = let (a,st') = f st in Yield a st'

take :: Int -> Stream a -> [a]
take len (Stream phi s0) = go len (phi s0)
  where
    go n _            | n < 1 = []
    go n (Yield a s1)         = a : go (n-1) (phi s1)
    go n (Skip s1)            = go n (phi s1)


drop :: Int -> Stream a -> Stream a
drop len (Stream phi s0) = Stream chi (SP3 C1 len s0)
  where
    chi (SP3 C1 n st) | n < 1 = Skip (SP3 C2 0 st)      -- switch to C2
    chi (SP3 C1 n st)         = case phi st of
        Skip s'    -> Skip (SP3 C1 n s')                -- remain in C1
        Yield _ s' -> Skip (SP3 C1 (n-1) s')            -- remain in C1, drop @a@

    chi (SP3 C2 n st)         = case phi st of
        Skip s'    -> Skip (SP3 C2 n s')                -- remain in C2
        Yield a s' -> Yield a (SP3 C2 n s')             -- remain in C2



-- | 'const' a:0:0:...
--
const :: Num a => a -> Stream a
const a = Stream phi a 
  where
    phi e = Yield e 0


