{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Tactus.Neume
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Manipulating meter patterns
--
--------------------------------------------------------------------------------


module Tactus.Neume
  (

    rationalToDuration

  ) where

import Tactus.Utils ( viewRational )

import Neume.Core.Duration

import Data.Ratio


-- | Convert a rational to a duration - dotting and double dotting
-- is supported.
rationalToDuration :: Rational -> Maybe Duration
rationalToDuration r | r == 4%1   = Just $ dLonga
                     | r == 2%1   = Just $ dBreve
                     | r == 0     = Just $ dZero
                     | r >  1     = Nothing
                     | otherwise  = let (n,d) = viewRational r
                                    in dotfun n d 
  where
    dotfun n d | n == 1    = fn d
               | n == 3    = fmap dot $ fn $ d `div` 2
               | n == 7    = fmap dot $ fn $ d `div` 4
               | otherwise = Nothing

    fn 1   = Just dWhole
    fn 2   = Just dHalf
    fn 4   = Just dQuarter
    fn 8   = Just dEighth
    fn 16  = Just dSixteenth
    fn 32  = Just dThirtySecondth
    fn _   = Nothing

