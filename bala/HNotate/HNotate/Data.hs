
--------------------------------------------------------------------------------
-- |
-- Module      :  HNotate.Data
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined
--
-- Data - e.g. label sets for key signatures
--
--------------------------------------------------------------------------------

module HNotate.Data where

import HNotate.CommonUtils
import HNotate.Duration
import HNotate.Pitch
import HNotate.MusicRepDatatypes

import Data.List


--------------------------------------------------------------------------------
-- PitchLabels
{-
7 sharps:   C#      A#m      G#Mix   D#Dor   E#Phr   F#Lyd   B#Loc
6 sharps:   F#      D#m      C#Mix   G#Dor   A#Phr   BLyd    E#Loc
5 sharps:   B       G#m      F#Mix   C#Dor   D#Phr   ELyd    A#Loc
4 sharps:   E       C#m      BMix    F#Dor   G#Phr   ALyd    D#Loc
3 sharps:   A       F#m      EMix    BDor    C#Phr   DLyd    G#Loc
2 sharps:   D       Bm       AMix    EDor    F#Phr   GLyd    C#Loc
1 sharp :   G       Em       DMix    ADor    BPhr    CLyd    F#Loc
0 sharps:   C       Am       GMix    DDor    EPhr    FLyd    BLoc
1 flat  :   F       Dm       CMix    GDor    APhr    BbLyd   ELoc
2 flats :   Bb      Gm       FMix    CDor    DPhr    EbLyd   ALoc
3 flats :   Eb      Cm       BbMix   FDor    GPhr    AbLyd   DLoc
4 flats :   Ab      Fm       EbMix   BbDor   CPhr    DbLyd   GLoc
5 flats :   Db      Bbm      AbMix   EbDor   FPhr    GbLyd   CLoc
6 flats :   Gb      Ebm      DbMix   AbDor   BbPhr   CbLyd   FLoc
7 flats :   Cb      Abm      GbMix   DbDor   EbPhr   FbLyd   BbLoc
-}

-- intervals?
labelSetOf :: Key -> Maybe LabelSet
labelSetOf (Key (PitchLabel l a) m xs)  = scaleSpelling l a m xs




scaleSpelling :: PitchLetter -> Accidental -> Mode -> [PitchLabel] -> Maybe LabelSet
scaleSpelling l a m accidentals = case elemIndex (l,a) $ modeLabels m of
    Just i -> Just $ makeLabelSet (7 - i) l accidentals
    Nothing -> Nothing
    
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
labelOrder a = let xs = take 15 $ dropWhile (/=a)  max_min_order'inf in
    snd $ foldr up (Flat,[]) xs
  where
    max_min_order'inf :: [PitchLetter]
    max_min_order'inf = cycle [B,E,A,D,G,C,F]

    up B (Nat, xs) = (Sharp,(B,Nat):xs)
    up B (Flat,xs) = (Nat,  (B,Flat):xs)
    up l (a,   xs) = (a,    (l,a):xs)
{-
down (Sharp,xs) F = (Nat, (F,Sharp):xs)
down (Nat  ,xs) F = (Flat,(F,Nat):xs)
down (a    ,xs) l = (a,   (l,a):xs)
-}




order_of_sharps :: [PitchLetter]
order_of_sharps = [F,C,G,D,A,E,B]

order_of_flats :: [PitchLetter]
order_of_flats = reverse order_of_sharps

makeLabelSet :: Int -> PitchLetter -> [PitchLabel] -> LabelSet
makeLabelSet i l accidentals 
    | i >= 0    = build sharp nat order_of_sharps i l
    | otherwise = build flat  nat order_of_flats  (abs i) l
  where
    build sk fk xs i =   labelSet
                       . map relabel 
                       . twist sk fk (take (mod i 8) xs)
                       . enumFromCyc                   

    nat l   = PitchLabel l Nat
    sharp l = PitchLabel l Sharp
    flat l  = PitchLabel l Flat
    
    -- for Klezmer or Hijaz ...
    -- if a pitch letter is in the set of accidentals then use the 
    -- labelling from accidentals 
    relabel :: PitchLabel -> PitchLabel
    relabel p@(PitchLabel l _) = maybe p id (find fn accidentals)
      where fn (PitchLabel l' _) = l' == l 


twist :: Eq a => (a -> b) -> (a -> b) -> [a] -> [a] -> [b]
twist sk fk ys ss = foldr step [] ss
  where
    step a acc | a `elem` ys = (sk a):acc
               | otherwise   = (fk a):acc  


--------------------------------------------------------------------------------
-- Meter patterns

four_four_of_eighth :: MeterPattern
four_four_of_eighth = ([4,4],eighth)



--------------------------------------------------------------------------------
-- Named elements

c_major'ls :: LabelSet 
c_major'ls = labelSet $ map (\a -> PitchLabel a Nat) $ enumFromCyc C
 
c_major :: Key
c_major = Key (PitchLabel C Nat) Major [] 


four_four :: Meter 
four_four = TimeSig 4 4


c_nat, d_nat, e_nat, f_nat, g_nat, a_nat, b_nat :: PitchLabel
c_nat     = PitchLabel C Nat
d_nat     = PitchLabel D Nat
e_nat     = PitchLabel E Nat
f_nat     = PitchLabel F Nat
g_nat     = PitchLabel G Nat
a_nat     = PitchLabel A Nat
b_nat     = PitchLabel B Nat

c_sharp, d_sharp, f_sharp, g_sharp, a_sharp :: PitchLabel
c_sharp   = PitchLabel C Sharp
d_sharp   = PitchLabel D Sharp
f_sharp   = PitchLabel F Sharp
g_sharp   = PitchLabel G Sharp
a_sharp   = PitchLabel A Sharp

d_flat, e_flat, g_flat, a_flat, b_flat :: PitchLabel
d_flat    = PitchLabel D Flat
e_flat    = PitchLabel E Flat
g_flat    = PitchLabel G Flat
a_flat    = PitchLabel A Flat
b_flat    = PitchLabel B Flat
