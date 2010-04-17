{-# LANGUAGE FlexibleInstances          #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Neume.Core.SpellingMap
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Pitch representation.
--
-- Pitch renaming for ABC.
--
--------------------------------------------------------------------------------

module Neume.Core.SpellingMap
  ( 

  -- * Pitch spelling
    SpellingMap
  , spell
  , makeSpellingMap

  ) where

import Neume.Core.Pitch

import qualified Data.Map as Map


-- | Make a @Pitch@ with a @PitchLabel@ and an @Octave@ designation.
makePitch :: PitchLabel -> Octave -> Pitch
makePitch (PitchLabel l a) o = Pitch l a o


-- | Drop the accidental of a @PitchLabel@.
root :: PitchLabel -> PitchLabel
root (PitchLabel l _) = PitchLabel l Nothing

-- | Change the accidental of a @PitchLabel@ making it a
-- natural (when printed the natural sign will be appear 
-- as a cautaionary accidental). 
natural :: PitchLabel -> PitchLabel
natural (PitchLabel l _) = PitchLabel l (Just Nat)


--------------------------------------------------------------------------------
-- Pitch spelling for ABC

{-
-- 7 sharps:   C#      A#m      G#Mix   D#Dor   E#Phr   F#Lyd   B#Loc
-- 6 sharps:   F#      D#m      C#Mix   G#Dor   A#Phr   BLyd    E#Loc
-- 5 sharps:   B       G#m      F#Mix   C#Dor   D#Phr   ELyd    A#Loc
-- 4 sharps:   E       C#m      BMix    F#Dor   G#Phr   ALyd    D#Loc
-- 3 sharps:   A       F#m      EMix    BDor    C#Phr   DLyd    G#Loc
-- 2 sharps:   D       Bm       AMix    EDor    F#Phr   GLyd    C#Loc
-- 1 sharp :   G       Em       DMix    ADor    BPhr    CLyd    F#Loc
-- 0 sharps:   C       Am       GMix    DDor    EPhr    FLyd    BLoc
-- 1 flat  :   F       Dm       CMix    GDor    APhr    BbLyd   ELoc
-- 2 flats :   Bb      Gm       FMix    CDor    DPhr    EbLyd   ALoc
-- 3 flats :   Eb      Cm       BbMix   FDor    GPhr    AbLyd   DLoc
-- 4 flats :   Ab      Fm       EbMix   BbDor   CPhr    DbLyd   GLoc
-- 5 flats :   Db      Bbm      AbMix   EbDor   FPhr    GbLyd   CLoc
-- 6 flats :   Gb      Ebm      DbMix   AbDor   BbPhr   CbLyd   FLoc
-- 7 flats :   Cb      Abm      GbMix   DbDor   EbPhr   FbLyd   BbLoc
-}



type SpellingMap = Map.Map PitchLabel PitchLabel


spell :: SpellingMap -> Pitch -> Pitch
spell sm p@(Pitch _ _ o) = makePitch (fn $ label p) o
  where
    fn lbl = maybe lbl id $ Map.lookup lbl sm

-- | Make a spelling map with @n@ accidentals. If @n@ is positive
-- the accidentals will be sharps, if @n@ s negative the 
-- accidentals will be flats.
makeSpellingMap :: Int -> SpellingMap
makeSpellingMap n 
    | abs n > 7 = error "Pitch.spellingMap - more sharps/flats than notes."
    | n == 0    = Map.empty
    | n >  0    = build $ nsharps n
    | otherwise = build $ nflats (abs n)          
  where
    build = foldr fn Map.empty where
      fn lbl m = Map.insert (root lbl) (natural lbl) 
               $ Map.insert lbl (root lbl) m

nsharps :: Int -> [PitchLabel]
nsharps n = map sharp $ take n [F,C,G,D,A,E,B] where
  sharp l = PitchLabel l (Just Sharp)

nflats :: Int -> [PitchLabel]
nflats n = map flat $ take n [B,E,A,D,G,C,F] where
  flat l = PitchLabel l (Just Flat)

