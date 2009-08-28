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

import Bala.Pitch hiding ( F )


data FretNum = X | F Int
  deriving (Eq,Ord)

type ChordDiagram = [FretNum]

x :: FretNum 
x = X

data Tuning = Tuning { 
      lowestString :: Pitch, 
      intervalSteps :: [Interval] 
    }
  deriving (Eq,Show)

instance Show FretNum where
  showsPrec _ X     = showChar 'x'
  showsPrec p (F i) = showsPrec p i

instance Num FretNum where
  (+) (F i) (F j) = F $ i+j
  (+) _     _     = X           -- X is an annihilator
  (*) (F i) (F j) = F $ i*j
  (*) _     _     = X           -- X is an annihilator
  signum = error "Fret numbers are not signed"
  abs    = error "Fret numbers are not signed"
  fromInteger i | i < 0     = X
                | otherwise = F $ fromInteger i

-- Moveable chords...
move :: Int -> ChordDiagram -> ChordDiagram
move i xs | all (/=0) xs = map (fromIntegral i +) xs
          | otherwise    = error "move - cannot move chords with open strings."


standardMarkup :: ChordDiagram -> String
standardMarkup xs = ($ "") . prefix (length xs) . foldr fn id $ zip snums xs
  where
    fn (i,j) f = shows i . showChar '-' . shows j . showChar ';' . f
    prefix i f = showString "w:" . shows i . showChar ';' . f
    snums = reverse [1.. length xs]

{-
pitchContent :: ChordDiagram -> Tuning -> [Pitch]
pitchContent xs t = []
-}
