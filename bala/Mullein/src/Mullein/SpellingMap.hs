{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Mullein.SpellingMap
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined
--
-- SpellingMap - a map from semitone count to pitch name.
-- Used by Abc for pitch spelling tones in a scale.
--
--------------------------------------------------------------------------------

module Mullein.SpellingMap (
  SpellingMap,
  rename,
  naturalize,
  makeSpellingMap,
  spellingMap,
  pitchNames,
  ) where

import Mullein.CoreTypes ( Key(..), Mode(..) )
import Mullein.Pitch
import Mullein.Utils

import Data.List ( elemIndex, find )
import qualified Data.Map as Map

--------------------------------------------------------------------------------
-- PitchLabels
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


newtype SpellingMap = SpellingMap { getSpellingMap :: Map.Map Int PitchLabel }
  deriving (Show)


rename :: SpellingMap -> Pitch -> Pitch
rename lbls p = maybe p id $ findPitchSpelling p lbls

-- Cancel the accidental if the pitch is found in the label set
-- This is the transformation needed for Abc: 
-- f# should be printed f in g major
naturalize :: SpellingMap -> Pitch -> Pitch
naturalize lbls p = maybe p nat (findPitchSpelling p lbls) where
    nat (Pitch l _ o) = Pitch l Nat o

-- Make a label set for the given key.
-- If the is key irregular (i.e. not in the table above) then the 
-- function returns Nothing.
makeSpellingMap :: Key -> [PitchLabel] -> Maybe SpellingMap
makeSpellingMap (Key (PitchLabel l a) m) modifiers
    = case elemIndex (l,a) $ modeLabels m of
        Just i  -> Just $ mkSM (7 - i) l modifiers
        Nothing -> Nothing


-- Create a spelling map /by hand/.
spellingMap :: [PitchLabel] -> SpellingMap
spellingMap = SpellingMap . foldl fn Map.empty
  where fn m p = Map.insert (semitones p) p m    

findPitchSpelling :: Pitch -> SpellingMap -> Maybe Pitch
findPitchSpelling (Pitch l a o) (SpellingMap m) = 
    maybe Nothing (fn o) (Map.lookup (semitones l + semitones a) m) 
  where
    fn ove (PitchLabel ltr atl) = Just $ Pitch ltr atl ove


pitchNames :: SpellingMap -> [PitchLabel]
pitchNames = Map.elems . getSpellingMap

modeLabels :: Mode -> [(PitchLetter, Accidental)]    
modeLabels Major        = labelOrder C
modeLabels Minor        = labelOrder A
modeLabels Mixolydian   = labelOrder G
modeLabels Dorian       = labelOrder D 
modeLabels Phrygian     = labelOrder E
modeLabels Lydian       = labelOrder F
modeLabels Locrian      = labelOrder B
modeLabels Ionian       = labelOrder C -- major
modeLabels Aeolian      = labelOrder A -- natural minor


-- 
labelOrder :: PitchLetter -> [(PitchLetter,Accidental)]
labelOrder letter = snd $ foldr up (Flat,[]) xs
  where
    xs :: [PitchLetter]
    xs = take 15 $ dropWhile (/=letter)  max_min_order'inf
    
    max_min_order'inf :: [PitchLetter]
    max_min_order'inf = cycle [B,E,A,D,G,C,F]

    up B (Nat, ys) = (Sharp,(B,Nat) : ys)
    up B (Flat,ys) = (Nat,  (B,Flat): ys)
    up l (a,   ys) = (a,    (l,a)   : ys)

mkSM :: Int -> PitchLetter -> [PitchLabel] -> SpellingMap
mkSM i letter modifiers
    | i >= 0    = build sharp nat order_of_sharps i letter
    | otherwise = build flat  nat order_of_flats  (abs i) letter
  where
    build sk fk xs n =   spellingMap
                       . map relabel
                       . twist sk fk (take (mod n 8) xs)
                       . enumFromCyc                   

    nat l   = PitchLabel l Nat
    sharp l = PitchLabel l Sharp
    flat l  = PitchLabel l Flat

    -- for Klezmer or Hijaz ...
    -- if a pitch letter is in the set of accidentals then use the 
    -- labelling from accidentals 

    relabel :: PitchLabel -> PitchLabel
    relabel p@(PitchLabel l _) = maybe p id (find fn modifiers)
      where fn (PitchLabel l' _) = l' == l 



order_of_sharps :: [PitchLetter]
order_of_sharps = [F,C,G,D,A,E,B]

order_of_flats :: [PitchLetter]
order_of_flats = reverse order_of_sharps

twist :: Eq a => (a -> b) -> (a -> b) -> [a] -> [a] -> [b]
twist sk fk ys ss = foldr step [] ss
  where
    step a acc | a `elem` ys = (sk a):acc
               | otherwise   = (fk a):acc      

