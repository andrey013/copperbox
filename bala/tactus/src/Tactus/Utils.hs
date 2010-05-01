{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Tactus.Utils
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Utils...
--
--------------------------------------------------------------------------------


module Tactus.Utils
  (

  -- * Numerator * denonimator
    viewRational

  -- * Hughes list
  , H
  , fromH
  , consH
  , replicateH

  ) where



import Data.Ratio

viewRational :: Rational -> (Integer,Integer)
viewRational r = (numerator r, denominator r)


--------------------------------------------------------------------------------


type H a = [a] -> [a]


fromH :: H a -> [a]
fromH = ($ [])

consH :: a -> H a
consH a = (a:)

replicateH :: Int -> a -> H a
replicateH n a = step n id where
  step i f | i <= 0    = f
           | otherwise = step (i-1) (f .  (a:))  

