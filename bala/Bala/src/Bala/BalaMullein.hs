{-# LANGUAGE FlexibleInstances          #-}
{-# OPTIONS -Wall #-}
{-# OPTIONS -fno-warn-orphans #-}

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
  
  -- * Constructors
  , mkNote
  , mkChord
  , mkRest

  -- ** LilyPond drums
  , mkDrumNote
  , mkDrumChord
  , makeDrumScore

  ) where

import qualified Bala.BeatPattern       as BP
import Bala.Duration
import Bala.Mullein.Abc
import Bala.Mullein.LilyPond
import Bala.Pitch

import qualified Mullein.Duration       as M 
import qualified Mullein.Extended       as M
import qualified Mullein.Pitch          as M
import qualified Mullein.Core           as M


import Data.Ratio

--------------------------------------------------------------------------------


instance M.HasDuration t => RationalDuration (t M.Duration) where
  rationalDuration = M.extent . M.getDuration



--------------------------------------------------------------------------------
-- Duration

toDuration :: Rational -> M.Duration
toDuration  = either restErr id . M.rationalToDuration 
  where
    restErr :: M.ConversionError -> M.Duration
    restErr e = error $ "balaDuration - cannot convert " 
                      ++ show (M.getBadRational e)
 

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


--------------------------------------------------------------------------------
-- 

mkNote :: M.MakeNote e => Pitch -> Rational -> e
mkNote p d = M.makeNote (toPitch p) (toDuration d)

mkChord :: M.MakeChord e => [Pitch] -> Rational -> e
mkChord ps d = M.makeChord (map toPitch ps) (toDuration d)

mkRest :: M.MakeRest e => Rational -> e
mkRest = M.makeRest . toDuration

-- LilyPond drums s

mkDrumNote :: M.DrumPitch -> Rational -> M.DrumGlyph
mkDrumNote p d = M.Note p (toDuration d) False


mkDrumChord :: [M.DrumPitch] -> Rational -> M.DrumGlyph
mkDrumChord ps d = M.Chord ps (toDuration d) False



-- Amalgamate beat patterns into a score
makeDrumScore :: Rational 
              -> Rational 
              -> [M.DrumPitch] 
              -> [BP.BeatPattern] 
              -> [M.DrumGlyph]
makeDrumScore timesig unitDuration dps patts = 
    map mkOne $ foldr (zipWith ($)) (repeat []) 
              $ map buildPitchLine 
              $ zip dps patts
  where

    buildPitchLine :: (M.DrumPitch,BP.BeatPattern) 
                   -> [[M.DrumPitch] 
                   -> [M.DrumPitch]]
    buildPitchLine (p,bp) = map fn $ BP.run1 timesig $ BP.unitBeat bp 
      where fn (BP.R _) = id
            fn (BP.B _) = (p:)

    mkOne []  = mkRest unitDuration
    mkOne [p] = mkDrumNote p unitDuration
    mkOne ps  = mkDrumChord ps unitDuration