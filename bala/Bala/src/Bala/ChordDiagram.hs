{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.ChordDiagram
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Chord diagrams for guitar
--
--------------------------------------------------------------------------------

module Bala.ChordDiagram where

import Bala.Interval
import Bala.NamedPitches ( e4 )
import Bala.Pitch

import Data.AffineSpace

import Data.Maybe

data FretNum = X | FN Int
  deriving (Eq,Ord)

newtype ChordDiagram = ChordDiagram { getChordDiagram :: [FretNum] }
  deriving (Eq)


x :: FretNum 
x = X

data Tuning = Tuning { 
      lowestString :: Pitch, 
      intervalSteps :: [Interval] 
    }
  deriving (Eq,Show)

instance Show FretNum where
  showsPrec _ X      = showChar 'x'
  showsPrec p (FN i) = showsPrec p i

instance Show ChordDiagram where
  showsPrec p (ChordDiagram xs) = showsPrec p xs



instance Num FretNum where
  (+) (FN i) (FN j) = FN $ i+j
  (+) _      _      = X           -- X is an annihilator
  (*) (FN i) (FN j) = FN $ i*j
  (*) _      _      = X           -- X is an annihilator
  signum = error "Fret numbers are not signed"
  abs    = error "Fret numbers are not signed"
  fromInteger i | i < 0     = X
                | otherwise = FN $ fromInteger i


makeChordDiagram :: [FretNum] -> ChordDiagram
makeChordDiagram = ChordDiagram

standardTuning :: Tuning
standardTuning = Tuning e4 [p4,p4,p4,mj4,p4]
  where
    p4  = makeInterval 4 5
    mj4 = makeInterval 3 4


-- Moveable chords...
move :: Int -> ChordDiagram -> ChordDiagram
move i (ChordDiagram xs) 
  | all (/=0) xs = ChordDiagram $ map (fromIntegral i +) xs
  | otherwise    = error "move - cannot move chords with open strings."


standardMarkup :: ChordDiagram -> String
standardMarkup (ChordDiagram xs) = 
    ($ "") . prefix (length xs) . foldr fn id $ zip snums xs
  where
    fn (i,j) f = shows i . showChar '-' . shows j . showChar ';' . f
    prefix i f = showString "w:" . shows i . showChar ';' . f
    snums = reverse [1.. length xs]


pc :: Tuning -> [Pitch]
pc (Tuning p ivls) = scanl (.+^) p ivls


-- A Fingering is interpreted as a semitone increment - it cannot 
-- be an interval as it has no notion of interval quality. So
-- the pitch list should be /re-spelled/.
pitchContent :: Tuning -> ChordDiagram -> [Pitch]
pitchContent t (ChordDiagram xs) = catMaybes $ zipWith fn xs (pc t) where
  fn X      _ = Nothing
  fn (FN i) p = Just $ p `addSemitones` i

-- Chord diagrams do not contain any pitch quality information.
-- So a @toChord@ function is not realistic (at least for the
-- representation of intervals used by Bala).

