{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Chord
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Chord representation 
--
--------------------------------------------------------------------------------

module Bala.Chord where

import Bala.Interval
import Bala.Pitch

import Data.AffineSpace

import Data.IntMap ( IntMap )
import qualified Data.IntMap as IM

data Chord = Chord { chordRoot :: Pitch, chordIntervals :: IntMap Int }
  deriving (Eq)


--------------------------------------------------------------------------------

instance Show Chord where
  showsPrec n ch@(Chord p _) = 
    shows p . showChar ':' . showsPrec n (map intervalName $ extractIntervals ch)


--------------------------------------------------------------------------------

extractIntervals :: Chord -> [Interval]
extractIntervals = map (uncurry makeInterval) . IM.toAscList . chordIntervals

chordPitches :: Chord -> [Pitch]
chordPitches ch@(Chord p _) = map (p .+^) $ extractIntervals ch

major :: Pitch -> Chord
major p = Chord p $ IM.fromList [(1,0), (3,4), (5,7)]


minor :: Pitch -> Chord
minor p = Chord p $ IM.fromList [(1,0), (3,3), (5,7)]


immap :: (IntMap Int -> IntMap Int) -> Chord -> Chord
immap f (Chord p ivls) = Chord p (f ivls)



maj7 :: Chord -> Chord
maj7 = immap (uncurry IM.insert $ intervalPair major7)  