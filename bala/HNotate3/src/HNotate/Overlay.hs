{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  HNotate.Overlay
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined
--
-- Overlay ...
--
--------------------------------------------------------------------------------



module HNotate.Overlay where

import HNotate.Duration

import Data.List ( foldl' )


-- An overlay is zipped onto the /prime/ sequence, if the overlay is longer
-- than the prime sequence it will be truncated.


-- Old ...

--------------------------------------------------------------------------------
-- grouping overlays

-- onset - change the onset time from /collapseTree/ to the bar number
-- Optional prefix a spacer - if the notes start mid-bar. 
onset :: (Temporal a, Spacer a) => 
    Duration -> [Duration] -> Seq a -> (Int,Seq a)
onset anacrusis ds0 notes = step 1 0 stk where
    stk   = reduceStk anacrusis ds0
    
    step bc n _       | n <= 0    = (bc, notes)   
    step bc n (d:ds)  | n < d     = (bc, spacer n <| notes)
                      | otherwise = step (bc+1) (n-d) ds
    step _  _ []                  = error $ "onset - duration stack exhausted"                       

mergeOverlays :: [(Int,[Bar a])] -> [Overlay a]
mergeOverlays (x:xs)  = foldl' zipOverlay (overlay1 x) xs
mergeOverlays []      = []

-- the first /line/ must be transformed to an overlay /by hand/, the other
-- /lines/ can then be zipped into it.
-- @start@ is bar number which starts at 1!
overlay1 :: (Int,[Bar a]) -> [Overlay a]
overlay1 (bar_num,xs) = map Single (replicate (bar_num-1) empty_bar ++ xs) where
    empty_bar = Bar []
    

zipOverlay :: [Overlay a] -> (Int,[Bar a]) -> [Overlay a]
zipOverlay xs (n,ys) 
    | n == 1     = longZipWith overl empty_sgl empty_bar xs ys 
    | n > 1      = longZipWith overl empty_sgl empty_bar xs (replicate n empty_bar ++ ys) 
    | otherwise  = error $ "zipOverlay - unreachable"
  where
    overl :: Overlay a -> Bar a -> Overlay a
    overl o          b   | nullBar b = o   -- no update
    overl (Single x) b   | nullBar x = Single b   -- swap
                         | otherwise = Multi [x,b]
    overl (Multi os) b               = Multi $ os++[b] -- yes really do an append!              
    
    nullBar (Bar [])      = True
    nullBar _             = False  
    empty_bar             = Bar []
    empty_sgl             = Single empty_bar        

                         