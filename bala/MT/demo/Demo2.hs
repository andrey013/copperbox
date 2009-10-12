{-# OPTIONS -Wall #-}

module Demo2 where

import IntervalMultiset
import Z12

ex1, ex2 :: [Z12]
ex1 = intervalMultiset [1,2,3]

ex2 = intervalMultiset [1,2,7,10]

ex1', ex2' :: [Int]
ex1' = intervalVector ex1
ex2' = intervalVector ex2

