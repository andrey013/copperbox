{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Tonos.Neume
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Neume
--
--------------------------------------------------------------------------------

module Tonos.Neume
  

   where

import Tonos.Pitch

import qualified Neume.Core.Pitch as Neume
-- import Neume.Core.SpellingMap


-- @standardForm@ might have 3 or more accidentals, which 
-- can\'t be representated in Neume.
-- 
-- So we need some enharmonic spelling...
--

neumePitch :: Pitch -> Neume.Pitch
neumePitch p = Neume.Pitch (neumePitchLetter l) (neumeAccidental a) o
  where
    (l,a,o)    = enharmonic $ standardForm p


-- could make this neater and avoid the Motion data type 
-- by having separate sharpen / flatten functions...
--
enharmonic :: (PitchLetter, Accidental, Octave) 
           -> (PitchLetter, Accidental, Octave)
enharmonic (l,a,o) = (lbl,acc,ove) 
  where
    (acc,lbl)  = if a >= 0 then sharpen a l else flatten a l

    direction  = motion a
    
    ove        = if cycled l lbl direction then upd o direction else o

    upd n SHARPEN = n+1
    upd n FLATTEN = n-1

data Motion = SHARPEN | FLATTEN

motion :: Int -> Motion
motion i | i >= 0    = SHARPEN
         | otherwise = FLATTEN  

cycled :: PitchLetter -> PitchLetter -> Motion -> Bool
cycled old new SHARPEN = if new < old then True else False
cycled old new FLATTEN = if new > old then True else False 



neumePitchLetter :: PitchLetter -> Neume.PitchLetter
neumePitchLetter C = Neume.C
neumePitchLetter D = Neume.D
neumePitchLetter E = Neume.E
neumePitchLetter F = Neume.F
neumePitchLetter G = Neume.G
neumePitchLetter A = Neume.A
neumePitchLetter B = Neume.B

neumeAccidental :: Accidental -> Maybe Neume.Accidental
neumeAccidental   0  = Nothing
neumeAccidental   1  = Just Neume.Sharp
neumeAccidental (-1) = Just Neume.Flat
neumeAccidental   _  = error $ "neumeAccidental - accidental not normalized"


sharpen :: Int -> PitchLetter -> (Int,PitchLetter)
sharpen i lbl | i > 1     = step i lbl
              | otherwise = (i,lbl)
  where
    step n C    = sharpen (n-2) D
    step n D    = sharpen (n-2) E
    step n E    = sharpen (n-1) F
    step n F    = sharpen (n-2) G
    step n G    = sharpen (n-2) A
    step n A    = sharpen (n-2) B
    step n B    = sharpen (n-1) C


flatten :: Int -> PitchLetter -> (Int,PitchLetter)
flatten i lbl | i < (-1)  = step i lbl
              | otherwise = (i,lbl)
  where
    step n C    = flatten (n+1) B
    step n D    = flatten (n+2) C
    step n E    = flatten (n+2) D
    step n F    = flatten (n+1) E
    step n G    = flatten (n+2) F
    step n A    = flatten (n+2) G
    step n B    = flatten (n+2) A

