{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.RetroInt
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Integer with /retrograde/ addition and no zero.
-- (Models calculations on pitch letter difference).
--
--------------------------------------------------------------------------------

module Bala.RetroInt
  ( 
  -- * /Retrograde/ Int
    RI

  , RDif(..)

  , toRI
  , fromRI

  ) where

import Data.Monoid
import Test.QuickCheck

-- Data types

newtype RI = RI Int
  deriving (Eq,Ord)

--------------------------------------------------------------------------------

-- | rdif is the analogue to subtraction on RetroInts, but it is 
-- the difference between the larger and the smaller and hence 
-- will always generate a positive answer.
class RDif a where
  rdif :: a -> a -> RI
 

instance RDif RI where
  rdif (RI a) (RI b) = RI $ max a b - ((min a b) - 1)


--------------------------------------------------------------------------------

instance Show RI where
  showsPrec p (RI i) = showsPrec p i

instance Arbitrary RI where 
  arbitrary = choose (1,100) >>= return . RI
  coarbitrary (RI i) = coarbitrary i

instance Monoid RI where
  mempty = RI 1
  mappend (RI a) (RI b) = RI $ a + (b-1)


--------------------------------------------------------------------------------
-- Conversion


toRI :: Int -> RI
toRI i | i > 0     = RI i
       | otherwise = error $ "toRI (<1) " ++ show i 

fromRI :: RI -> Int
fromRI (RI i) = i



