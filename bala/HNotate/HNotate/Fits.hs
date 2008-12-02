{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  HNotate.CommonUtils
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  mptc, fundeps
--
-- 'Fitting' and measuring.
--
--------------------------------------------------------------------------------

module HNotate.Fits where

-- Should be no deps on other HNotate modules

import qualified Data.Foldable as F
import Data.Sequence hiding (reverse)
import Prelude hiding (null, length)



--------------------------------------------------------------------------------
-- 'Fitting' 

data Fit a = Fit a | Split a a | AllRight a
  deriving Show

-- Note (30/11/08) 
-- To handle nplets (i.e. tuplets) resizeTo might have to be factored into
-- resizeLeftTo & resizeRightTo
class (Ord b, Num b) => Fits a b | a -> b where
  measure   :: a -> b
  resizeTo  :: a -> b -> a

instance Fits Int Int where 
  measure = id
  resizeTo _ b = b 

total :: (Fits a b) => Fit a -> b  
total (Fit a)       = measure a
total (Split a b)   = measure a + measure b
total (AllRight b)  = measure b


fits :: Fits a b => a -> b -> Fit a
fits a i  | measure a <= i  = Fit a
          | i <= 0          = AllRight a
          | otherwise       = Split (resizeTo a i) (resizeTo a (measure a - i)) 

sumMeasure :: (Fits a b, F.Foldable c) => c a -> b
sumMeasure = F.foldr (\e a -> measure e + a) 0 

fitsSeq :: Fits a b => Seq a -> b -> Fit (Seq a)
fitsSeq = fitsSeqHy id 



fitsSeqHy :: (Fits a b) => (Seq a -> Seq a) -> Seq a -> b -> Fit (Seq a)
fitsSeqHy hyphenate sa i 
    | i <= 0      = AllRight sa
    | otherwise   = step (empty,0) (viewl sa)
  where 
    step (acc,_) EmptyL     = Fit acc
    step (acc,n) (e :< se)  = case fits e (i - n) of
            Fit a      -> step (acc |> a, n + measure e) (viewl se) 
            Split a b  -> Split (hyphenate $ acc |> a) (b <| se)
            AllRight _ -> if null acc then AllRight (e <| se)
                                      else Split acc (e <| se) 

                    
sumSegments :: Fits a b => Seq (Seq a) -> b
sumSegments = F.foldr (\a n -> sumMeasure a + n) 0
                                  
-- Only do segmenting on sequences, it's getting too exotic to do on 
-- other containers.
segment :: Fits a b => b -> Seq a -> Seq (Seq a)
segment n se | n > 0      = asegment 0 n se  
             | otherwise  = error $ "segment - segment divisor must be >0"


-- Segment with an anacrusis - i.e. the first section is a different 
-- length to the following sections. In music the anacrusis will be smaller
-- than the following sections but we do not enforce that here. 
-- (Note that the anacrusis can be 0).
asegment :: Fits a b => b -> b -> Seq a -> Seq (Seq a)
asegment = asegmentHy id

 
-- The most general segment function: 
-- The first segment length 'a' is an anacrusis (it doesn't have to be 
-- the same length as the following section).
-- Also the function uses a hyphenate function this is used on the left section
-- when a split has divided an element. If the sequence contained words this 
-- could be used to add a hyphen to the breaking line, for music it means we 
-- can add a tie when we have broken a note that spans two bars.

asegmentHy :: Fits a b => (Seq a -> Seq a) -> b -> b -> Seq a -> Seq (Seq a)
asegmentHy hyphenate asis n se  
    | n <= 0      = error $ "segmenting - the divisor must be >0"
    | otherwise   = let (ana_segment, rest) = firstStep se
                    in step ana_segment rest               
  where
    -- firstStep :: Fits a b => Seq a -> (Seq (Seq a), Seq a)
    firstStep sa = case fitsSeqHy hyphenate sa asis of
                    Fit a       -> (singleton a, empty)
                    Split a b   -> (singleton a, b)
                    AllRight b  -> (empty,       b) 
                    
    step acc sa = case fitsSeqHy hyphenate sa n of
                    Fit a       -> acc |> a
                    Split a b   -> step (acc |> a) b -- to keen...
                    AllRight _  -> error "unreachable" -- (n<=0) guard stops this 

{-

-- This is the type signature we want, it needs the fundep on Fits
prop_split_eq_total :: (Fits a b) => b -> a -> Bool
prop_split_eq_total split a = measure a == total (fits a split)

-}
 