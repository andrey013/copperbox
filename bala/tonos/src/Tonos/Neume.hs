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

import Tonos.Base
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
    (acc,lbl)  = if a >= 0 then l `sharpenBy` a else l `flattenBy` a

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


sharpenBy :: PitchLetter -> Int -> (Int,PitchLetter)
sharpenBy ltr i | i > 1     = next `sharpenBy` (i - distance ltr next)
                | otherwise = (i,ltr)
  where
    next = ltr+1

-- flattenBy takes a negative number and takes it towards (-1) ...
--
flattenBy :: PitchLetter -> Int -> (Int,PitchLetter)
flattenBy ltr i | i < (-1)  = next `flattenBy` (i + distance ltr next)
                | otherwise = (i,ltr)
  where
    next = ltr-1