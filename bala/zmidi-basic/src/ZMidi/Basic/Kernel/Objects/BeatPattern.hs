{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZMidi.Basic.Kernel.Objects.BeatPattern
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Output via a trace (i.e a writer monad).
--
--------------------------------------------------------------------------------

module ZMidi.Basic.Kernel.Objects.BeatPattern
  ( 
    BeatPattern 
  , beats

  , para
  , sequ
   
  ) where


import ZMidi.Basic.Utils.JoinList

import Data.Monoid


newtype BeatPattern = BP { measures :: JoinList BeatMeasure }
  deriving Show

data BeatMeasure    = BM { width :: Int, overlays :: JoinList BeatVector }
  deriving Show

data BeatVector     = BV { delay :: Int, vec :: [Int] }
  deriving Show

beats :: [Int] -> BeatPattern
beats ns = BP $ one (BM w (one bv))
  where
    bv = BV 0 ns 
    w  = bvWidth bv
                


bvWidth :: BeatVector -> Int
bvWidth bv = delay bv + sum (vec bv)

expand :: Int -> BeatVector -> BeatVector
expand n bv = if n < w then bv 
                       else let v = vec bv in bv { vec = ext v (n - w) }
  where
    w            = bvWidth bv
    ext []     i = [i]
    ext (x:xs) i = x : ext xs i


sequ :: BeatPattern -> BeatPattern -> BeatPattern
sequ (BP ja) (BP jb) = BP $ ja `mappend` jb

para :: BeatPattern -> BeatPattern -> BeatPattern
para (BP ja) (BP jb) = BP $ step (viewl ja) (viewl jb)
  where
    step EmptyL    EmptyL     = mempty     
    step (a :< sa) EmptyL     = a `cons` sa
    step EmptyL    (b :< sb)  = b `cons` sb
    step (a :< sa) (b :< sb)  = para1 a b `cons` step (viewl sa) (viewl sb)

para1 :: BeatMeasure -> BeatMeasure -> BeatMeasure
para1 (BM i sa) (BM j sb) 
    | i >  j    = let sb' = fmap (expand i) sb in BM i (sa `mappend` sb')
    | i <  j    = let sa' = fmap (expand j) sa in BM j (sa' `mappend` sb)
    | otherwise = BM i (sa `mappend` sb)
 
