{-# LANGUAGE MultiParamTypeClasses #-} 

--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Base.BaseExtra
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Useful functions
--
--------------------------------------------------------------------------------


module Bala.Base.BaseExtra (
 
  -- * Type classes
  Increment(..),
  Displacement(..),
  Synonym(..),
  
  -- * Counting
  sentinelCount,

  
  base2bases, log2whole,
  
  -- * Utility functions 
  dup, ffst, fsnd, fork, prod,
  
  applyi, zam, 
  mod12, mod7,
  sub, sub1, 
  andthen, ora, anda,
  dyap, triap,   {- dyapr, dyapl, dycl, dycr, -}
  hexStr, 
  
  -- ** Helpers for modulo 12 and modulo 100.
  -- $explodedoc 
  explode12, explode100, collapse12, collapse100,
  normalize12, normalize100, 
  
  -- ** transpose and other functions on sequences
  stranspose, 
  szip, szipWith,
  szipl, sziplWith, szipl', 
  sreplicate, smaximum, sminimum,
  sconcat, sfilter

  ) where



import qualified Data.Foldable as F
import Data.List (unfoldr)
import Data.Sequence
import Numeric (showHex)
import Prelude hiding (null)



--------------------------------------------------------------------------------
-- Increment 

-- Change a value by a value of another type
-- e.g. Pitches are changed by increasing the number of semitones

class Increment a b where
  increase :: a -> b -> a
  decrease :: a -> b -> a


--------------------------------------------------------------------------------
-- Displacement

-- What is the displacement (distance) between two values?
-- e.g. C to G is +7 semitones 


class Displacement a b where
  displacement :: a -> a -> b

  
--------------------------------------------------------------------------------
-- Synonym - e.g. B# & C, diminished fifth & augmented fourth

class Synonym a where
  synonym :: a -> a -> Bool
  
  
--------------------------------------------------------------------------------
-- Counting 

-- A 'fence post' count - count the sentinels at each end
-- Also, the measure is solely distance, direction isn't considered    
sentinelCount :: (Num a, Ord a) => a -> a -> a 
sentinelCount i j  | j > i      = f j i
                   | otherwise  = f i j
                    
                    
  where f i j = i - j + 1   
  
  

-- | generate an infinite list of base2 bases (?) 
-- (could filter when logBase 2 i generates whole numbers)
base2bases :: [Integer]
base2bases = unfoldr (\x -> Just (x, x * 2)) 1 


log2whole :: Integral a => a -> Bool
log2whole i = f i == 0 where
  f = snd . properFraction . logBase 2 . fromIntegral

--------------------------------------------------------------------------------
-- Utility functions

dup :: a -> (a,a)
dup a = (a,a)

ffst :: (a -> c) -> (a,b) -> (c,b)
ffst f (a,b) = (f a, b)

fsnd :: (b -> c) -> (a,b) -> (a,c)
fsnd f (a,b) = (a, f b)

fork :: (a -> b) -> (a -> c) -> a -> (b,c) 
fork f g a = (f a, g a)

prod :: (a -> c) -> (b -> d) -> (a,b) -> (c,d)
prod f g (a,b) = (f a, g b) 


-- | Apply a function i times.              
applyi :: Int -> (a -> a) -> a -> a
applyi i f a | i <= 0    = a
             | otherwise = applyi (i-1) f (f a) 
             
             
-- | zam - zippy map.
zam :: (a -> a -> b) -> [a] -> [b]
zam f (x:y:ys) = f x y : zam f (y:ys)
zam f _        = []

-- | mod12 - modulus 12.
mod12 :: (Integral a) => a -> a
mod12 i = i `mod` 12

-- | mod12 - modulus 7.
mod7 :: (Integral a) => a -> a
mod7  i = i `mod` 7  

-- | sub - flipped (-) 
sub :: Num a => a -> (a -> a)
sub = flip (-)

-- | sub1 - subtract 1. 
sub1 :: Num a => a -> a 
sub1 = sub 1

-- | Apply @f@ then @g@ to @a@ combining the results with @op@.
-- 
-- > andthen op f g a = f a `op` g a 
andthen :: (b -> c -> d) -> (a -> b) -> (a -> c) -> a -> d
andthen op f g a = f a `op` g a 

-- | (||) with 'apply' - test a with f, if it fails test it with g.
ora :: (a -> Bool) -> (a -> Bool) -> a -> Bool
ora = andthen (||) 

-- | (&&) with 'apply' - test a with both f and g.
anda :: (a -> Bool) -> (a -> Bool) -> a -> Bool
anda = andthen (&&)


-- | Dyadic apply \/ compose - apply the binary function g to a and b, 
-- then apply the unary function f to the result.
dyap :: (c -> d) -> (a -> b -> c) -> a -> b -> d
dyap f g a b = f (g a b) 

-- dyapr :: (c -> d) -> (a -> b -> c) -> a -> b -> d
dyapl f g a b = f a (g b) 

dyapr f g a b = f (g a) b 

dycl f g a b = g (f a) b 

dycr f g a b = g a (f b)
 

-- | Triadic apply \/ compose - apply the ternary function g to a, b and c, 
-- then apply the unary function f to the result.
triap :: (d -> e) -> (a -> b -> c -> d) -> a -> b -> c -> e
triap f g a b c = f (g a b c) 

-- | Show as a hexadecimal string, prefixed with @0x@.
hexStr :: Integral a => a -> String
hexStr a = (showString "0x" . showHex a) []

 
--------------------------------------------------------------------------------
-- Helpers for modulo 12 and modulo 100.

-- $explodedoc
-- Useful for semitone arithmetic (base 12) and cent arithmetic (base 100).
-- The divMod pair makes a /fraction/. 

-- | divMod for base 12.              
explode12 :: (Integral a) => a -> (a, a) 
explode12 i       = i `divMod` 12

-- | divMod for base 100. 
explode100  :: (Integral a) => a -> (a, a)
explode100 i      = i `divMod` 100

-- | Collapse an 'explode12' pair back to an integral.
collapse12  :: (Integral a) => (a, a) -> a
collapse12 (o,d)  = d + 12 * o

-- | Collapse an 'collapse100' pair back to an integral.
collapse100  :: (Integral a) => (a, a) -> a
collapse100 (o,d) = d + 100 * o

-- | Normalize an 'explode12' pair so that the right hand size is less than 12.
normalize12 :: (Integral a) => (a, a) -> (a, a) 
normalize12 (o,d)  = let (c, d') = explode12 d in (o + c, d')

-- | Normalize an 'explode100' pair so that the right hand size is less than 100.
normalize100 :: (Integral a) => (a, a) -> (a, a) 
normalize100 (o,d) = let (c, d') = explode100 d in (o + c, d')

  
--------------------------------------------------------------------------------
-- transpose for sequences



stranspose :: Seq (Seq a) -> Seq (Seq a)
stranspose = step . viewl
  where
    step EmptyL         = empty
    step (x :< sse)     = case viewl x of
        EmptyL    -> stranspose sse
        (e :< se) -> (e <| allheads sse) <| (stranspose $ se <| alltails sse)
    
    allheads = F.foldl' (\acc se -> acc |> head1 se)  empty
    alltails = F.foldl' (\acc se -> acc |> tail1 se)  empty
    
    head1 se = case viewl se of EmptyL -> unmatchErr; 
                                (a :< _) -> a
    
    tail1 se = case viewl se of EmptyL -> unmatchErr; 
                                (_ :< sa) -> sa
                                
    unmatchErr = error "stranspose - sequences of unequal length"                            

szip :: Seq a -> Seq b -> Seq (a,b)
szip = szipWith (,)

-- second seq can be longer than first (but gets truncated) 
-- behaviour as per zip 
szipWith :: (a -> b -> c) -> Seq a -> Seq b -> Seq c
szipWith f sa sb = step (viewl sa) (viewl sb) where
    step (a :< sa) (b :< sb)  = (f a b) <| step (viewl sa) (viewl sb)
    step EmptyL    EmptyL     = empty
    step EmptyL    _          = empty
    step _         EmptyL     = error "szipWith - right sequence short"
    
    
-- Zip a sequence and a list - list can be infinite
-- Zip a sequence and a list - list can be infinite
szipl :: Seq a -> [b] -> Seq (a,b)
szipl = sziplWith (,)


sziplWith :: (a -> b -> c) -> Seq a -> [b] -> Seq c
sziplWith f se xs = step (viewl se) xs where
  step (a :< se) (x:xs)   = f a x <| step (viewl se) xs
  step EmptyL   []        = empty
  step EmptyL    _        = empty
  step _         []       = error "sziplWith - list too short"
  


-- The sequence is primary - i.e. its an error if it runs out first, 
-- the pair is flipped so the list item is fst.
szipl' :: Seq a -> [b] -> Seq (b,a)
szipl' = sziplWith (flip (,))
  
sreplicate :: Int -> a -> Seq a
sreplicate i a | i <= 0     = empty
               | otherwise  = a <| sreplicate (i-1) a 
  
smaximum :: (Ord a) => (Seq a) -> a
smaximum se | null se = error "smaximum: empty sequence"
            | otherwise = let (a :< sa) = viewl se in F.foldr max a se 

sminimum :: (Ord a) => (Seq a) -> a
sminimum se | null se = error "sminimum: empty sequence"
            | otherwise = let (a :< sa) = viewl se in F.foldr min a se

sconcat :: Seq (Seq a) -> Seq a
sconcat = F.foldr (><) empty
             
sfilter :: (a -> Bool) -> Seq a -> Seq a
sfilter pf = step . viewl where
  step EmptyL     = empty
  step (a :< sa) | pf a       = a <| step (viewl sa)
                 | otherwise  = step (viewl sa)
                 
                         
            