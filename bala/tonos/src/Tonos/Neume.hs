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

enharmonic :: (PitchLetter, Accidental, Octave) 
           -> (PitchLetter, Accidental, Octave)
enharmonic (l,a,o) = (l', a', odif+o)
  where
    (odif,n) = (a + semitones l) `divMod` 12
    (l',a')  = spell n 

-- Need a 'proper' spelling map...
spell 0 = (C,0)
spell 1 = (C,1)
spell 2 = (D,0)
spell 3 = (D,1)
spell 4 = (E,0)
spell 5 = (F,0)
spell 6 = (F,1)
spell 7 = (G,0)
spell 8 = (G,1)
spell 9 = (A,0)
spell 10 = (A,1)
spell _  = (B,0)

-- superceeded (almost ...)
enharmonic' :: (PitchLetter, Accidental, Octave) 
            -> (PitchLetter, Accidental, Octave)
enharmonic' (l,a,o) = (lbl,acc,ove) 
  where
    (lbl,acc)  = if a >= 0 then l `sharpenBy` a else l `flattenBy` a

    direction  = motion a
    
    ove        = if cycled l lbl direction then upd o direction else o

    upd n SHARPEN = n+1
    upd n FLATTEN = n-1



data Motion = SHARPEN | FLATTEN

motion :: Accidental -> Motion
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


sharpenBy :: PitchLetter -> Accidental -> (PitchLetter,Accidental)
sharpenBy ltr i | i > 1     = next `sharpenBy` (i - distance ltr next)
                | otherwise = (ltr,i)
  where
    next = ltr+1

-- flattenBy takes a negative number and takes it towards (-1) ...
--
flattenBy :: PitchLetter -> Accidental -> (PitchLetter,Accidental)
flattenBy ltr i | i < (-1)  = next `flattenBy` (i + distance ltr next)
                | otherwise = (ltr,i)
  where
    next = ltr-1