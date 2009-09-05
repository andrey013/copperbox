{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.BalaMullein
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Interface to Mullein (pitch conversion, duration conversion).
--
--------------------------------------------------------------------------------

module Bala.BalaMullein
  ( 
  -- * Re-export internal modules
    module Bala.Mullein.Abc
  , module Bala.Mullein.LilyPond

  -- * Conversion
  , toDuration
  , toPitch

  ) where

import Bala.Mullein.Abc
import Bala.Mullein.LilyPond
import Bala.Pitch

import Mullein.Duration
import qualified Mullein.Pitch as M

import Data.Ratio

--------------------------------------------------------------------------------
-- Duration

toDuration :: Rational -> Duration
toDuration  = either restErr id . rationalToDuration 
  where
    restErr :: ConversionError -> Duration
    restErr e = error $ "balaDuration - cannot convert " 
                      ++ show (getBadRational e)
 

--------------------------------------------------------------------------------
-- Pitch

-- Mullein does not support /excessive/ accidents (e.g. triple flat)

toPitch :: Pitch -> M.Pitch
toPitch p@(Pitch l a o) 
    | a > 2 || a < (-2) = toPitch $ fromSemitones $ toSemitones p
    | otherwise         = M.Pitch (toPitchLetter l) (toAccidental a) o

toPitchLetter :: PitchLetter -> M.PitchLetter
toPitchLetter = toEnum . fromEnum

toAccidental :: Int -> Maybe M.Accidental
toAccidental n | n == (-2) = Just M.DoubleFlat
               | n == (-1) = Just M.Flat
               | n == 1    = Just M.Sharp
               | n == 2    = Just M.DoubleSharp
               | otherwise = Nothing
