{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  IntervalMultiset
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Interval multiset
--
--------------------------------------------------------------------------------

module IntervalMultiset
  ( 
  
  -- * Operations
    intervalMultiset
  , intervalVector
  ) where


import Z12

import Data.List ( sort )

intervalMultiset :: [Z12] -> [Z12]
intervalMultiset = sort . map minpair . allpairs
  where
    minpair (a,b) = min (a-b) (b-a)

allpairs :: [a] -> [(a,a)]
allpairs xs0 = concat $ step xs0 
  where 
    step (x:xs) = map (pair x) xs : step xs
    step []     = []
    pair x y    = (x,y)


    
intervalVector :: [Z12] -> [Int]
intervalVector = untup . foldr fn (0,0,0,0,0,0)
  where 
    fn 1 (a,b,c,d,e,f) = (a+1,b,c,d,e,f)
    fn 2 (a,b,c,d,e,f) = (a,b+1,c,d,e,f)
    fn 3 (a,b,c,d,e,f) = (a,b,c+1,d,e,f)
    fn 4 (a,b,c,d,e,f) = (a,b,c,d+1,e,f)
    fn 5 (a,b,c,d,e,f) = (a,b,c,d,e+1,f)
    fn 6 (a,b,c,d,e,f) = (a,b,c,d,e,f+1)
    fn i _             = error $ "intervalVector digit - " ++ show i
    untup (a,b,c,d,e,f) = [a,b,c,d,e,f]
