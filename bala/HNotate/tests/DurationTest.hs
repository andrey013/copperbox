
-- ghc ...
-- :set -i..:../HNotate
module DurationTest where

import HNotate.Duration

import Data.Ratio

type RatioInt = Ratio Int





{-
representableDuration :: Ratio Int -> Duration
representableDuration r 
    | r > 8%1 || r < (1%128) = unrepErr r
    | otherwise              = let rt = root r in dot (duration rt) rt 0
                              
  where
    rec_lim     = 3
    root r      = head $ dropWhile (r<) decreasing_ratios
    
    dot sk r' i | r' == r      = sk i
                | i > rec_lim  = unrepErr r
                | otherwise    = dot sk (augment r') (i+1)
                   
    augment r   = r + (1 % (denominator r *2))                 
    unrepErr r  = error $ "Unrepresentable duration " ++ show r
    
    
decreasing_ratios :: [Ratio Int]
decreasing_ratios = 4%1 : 2%1 : xs
  where xs = map (1 %) base2number_sequence
  
-}

