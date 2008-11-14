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
import Data.Monoid
import Data.Sequence hiding (reverse)
import Prelude hiding (null, length)



--------------------------------------------------------------------------------
-- 'Fitting' 

data Fit a = Fit a | Split a a | AllRight a
  deriving Show

class (Ord b, Num b) => Fits a b | a -> b where
  measure   :: a -> b
  resizeTo  :: a -> b -> a

instance Fits Int Int where 
  measure = id
  resizeTo _ b = b 

total :: (Fits a b) => Fit a -> b  
total (Fit a)     = measure a
total (Split a b) = measure a + measure b

fits :: Fits a b => a -> b -> Fit a
fits a i  | measure a <= i  = Fit a
          | i <= 0          = AllRight a
          | otherwise       = Split (resizeTo a i) (resizeTo a (measure a - i)) 

sumMeasure :: (Fits a b, F.Foldable c) => c a -> b
sumMeasure = F.foldr (\e a -> measure e + a) 0 

fitsSeq :: Fits a b => Seq a -> b -> Fit (Seq a)
fitsSeq = fitsGen (|>) null 


-- 'Fits' for lists is a bit convoluted - if fitsGen used a right fold 
-- fitsList wouldn't need the reverses, but we would have to do sumMeasure 
-- first in fitsGen and then a countdown.
-- As HNotate uses Seq for must collections, I've made the code simplest 
-- for the common case.  
   
fitsList :: Fits a b => [a] -> b -> Fit [a]
fitsList xs n = case fitsGen (flip (:)) isEmpty xs n of
    Fit xs -> Fit (reverse xs)
    Split xs ys -> Split (reverse xs) (reverse ys)  
  where
    -- null has been hidden...
    isEmpty [] = True
    isEmpty xs = False  
  

fitsGen :: (Fits a b, F.Foldable c, Monoid (c a)) => 
    (c a -> a -> c a) -> (c a -> Bool) -> c a -> b -> Fit (c a)
fitsGen add predEmpty sa i 
    | i <= 0      = AllRight sa
    | otherwise   = mkSplit $ F.foldl' fn (0,mempty,mempty) sa
  where 
    mkSplit (_,l,r) = if predEmpty r then Fit l else Split l r   -- 
      
    fn (n,l,r) e 
        | n >= i     = (n,l,r `add` e)  -- don't care about adding measure e to n  
        | otherwise  = let ef = fits e (i - n) in 
                       case ef of 
                         Fit a -> (n + measure e, l `add` a, r)
                         Split a b -> (n + measure e, l `add` a, r `add` b)
                         AllRight b -> (n, l, r `add` b)  


                    
sumSections :: Fits a b => Seq (Seq a) -> b
sumSections = F.foldr (\a n -> sumMeasure a + n) 0
                                  
-- Only do sectioning on sequences, it's getting too exotic to do on 
-- other containers.
section :: Fits a b => Seq a -> b -> Seq (Seq a)
section se n | n > 0      = asection se 0 n 
             | otherwise  = error $ "section - section divider must be >0"


-- Section with an anacrusis - i.e. the first section is a different 
-- length to the following sections. In music the anacrusis will be smaller
-- than the following sections but we do not enforce that here. 
-- (Note that the anacrusis can be 0).
asection :: Fits a b => Seq a -> b -> b -> Seq (Seq a)
asection = asectionHy id

 
-- The most general section function: 
-- The first section length 'a' is an anacrusis (it doesn't have to be 
-- the same length as the following section).
-- Also the function uses a hyphenate function this is used on the left section
-- when a split has divided an element. If the sequence contained words this 
-- could be used to add a hyphen to the breaking line, for music it means we 
-- can add a tie when we have broken a note that spans two bars.

asectionHy :: Fits a b => (Seq a -> Seq a) -> Seq a -> b -> b -> Seq (Seq a)
asectionHy hyphenate se a n 
    | n <= 0      = error $ "sectioning - the divider must be >0"
    | otherwise   = let (ana_section, rest) = firstStep se
                    in step ana_section rest               
  where
    -- firstStep :: Fits a b => Seq a -> (Seq (Seq a), Seq a)
    firstStep se = case fitsSeq se a of
                    Fit a       -> (singleton a, empty)
                    Split a b   -> (singleton $ hyphenate a, b)
                    AllRight b  -> (empty,       b) 
  
    step acc se = case fitsSeq se n of
                    Fit a       -> acc |> a
                    Split a b   -> step (acc |> hyphenate a) b
                    AllRight b  -> error "unreachable" -- (n<=0) guard stops this 

 