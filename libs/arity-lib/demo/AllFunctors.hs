{-# OPTIONS -Wall #-}

module Functors where

import Control.Monad.Instances ()  -- bring in Functor instance for (a ->)

import Data.Arity.Functors

f1 :: Int -> Int -> Int
f1 = abs `oo` (*)

demo1 :: Int
demo1 = f1 4 (-9)

max3 :: Int -> Int -> Int -> Int
max3 a b c = max a (max b c)

demo2 :: Int
demo2 = f 3 10 8 where f = negate `ooo` max3

