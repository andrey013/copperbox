{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  HNotate.Data
-- Copyright   :  (c) Stephen Tetley 2009
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


import HNotate.Duration
import HNotate.Pitch
import HNotate.NamedElements 
import HNotate.MusicRepDatatypes
import HNotate.Utils

import Data.List
import Data.Ratio


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

labelSetOf' :: Key -> LabelSet
labelSetOf' = maybe default_labelset id . labelSetOf


scaleSpelling :: 
    PitchLetter -> Accidental -> Mode -> [PitchLabel] -> Maybe LabelSet
scaleSpelling l a m accidentals = case elemIndex (l,a) $ modeLabels m of
    Just i -> Just $ makeLabelSet (7 - i) l accidentals
    Nothing -> Nothing

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





order_of_sharps :: [PitchLetter]
order_of_sharps = [F,C,G,D,A,E,B]

order_of_flats :: [PitchLetter]
order_of_flats = reverse order_of_sharps

makeLabelSet :: Int -> PitchLetter -> [PitchLabel] -> LabelSet
makeLabelSet i letter accidentals 
    | i >= 0    = build sharp nat order_of_sharps i letter
    | otherwise = build flat  nat order_of_flats  (abs i) letter
  where
    build sk fk xs n =   labelSet
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
    relabel p@(PitchLabel l _) = maybe p id (find fn accidentals)
      where fn (PitchLabel l' _) = l' == l 


twist :: Eq a => (a -> b) -> (a -> b) -> [a] -> [a] -> [b]
twist sk fk ys ss = foldr step [] ss
  where
    step a acc | a `elem` ys = (sk a):acc
               | otherwise   = (fk a):acc  


--------------------------------------------------------------------------------
-- Meter patterns

defaultMeterPattern :: Meter -> MeterPattern
defaultMeterPattern CommonTime    = ([4,4], eighth)
defaultMeterPattern CutTime       = ([2,2], eighth)
defaultMeterPattern (TimeSig n d) 
    | compoundMeter (n,d)         = (patt 3 (n%d), eighth)
    | simpleMeter (n,d)           = (patt 2 (n%d), eighth)
    | otherwise                   = (badpatt (n%d), eighth)
  where
    patt :: Integer -> Rational -> [Integer]
    patt i r = fmap floor $ replicate (fromIntegral i) $ (r / (i%1)) / (1%8) 
    
    -- divide into eigths
    badpatt :: Rational -> [Integer]
    badpatt r = replicate (ceiling $ r / (1%8)) 1
      
mkMeterPattern :: Meter -> [Duration]
mkMeterPattern CommonTime    = replicate 4 quarter
mkMeterPattern CutTime       = replicate 2 quarter
mkMeterPattern (TimeSig n d) 
    | simpleMeter (n,d)           = map (.* eighth) $ meterDivisions 2 (n%d)
    | compoundMeter (n,d)         = map (.* eighth) $ meterDivisions 3 (n%d)
    | otherwise                   = error $ "mkMeterPattern"


(.*) :: Integer -> Rational -> Rational
(.*) i r = i%1 * r

-- see Gardner Read 'Music Notation' p.169
meterDivisions :: Integer -> Rational -> [Integer]
meterDivisions i r = map floor $ replicate (fromIntegral i) $ (r / (i%1)) / (1%8) 
    
log2whole :: Integral a => a -> Bool
log2whole i = f i == 0 where
    f = snd . pf . logBase 2 . fromIntegral
    
    pf :: Double -> (Int, Double)
    pf = properFraction

compoundMeter :: (Integer,Integer) -> Bool
compoundMeter (n,d) = log2whole d && (n `mod` 3 == 0)

                      
simpleMeter :: (Integer,Integer) -> Bool
simpleMeter (_,d) = log2whole d

four_four_of_eighth :: MeterPattern
four_four_of_eighth = ([4,4],eighth)


--------------------------------------------------------------------------------
-- Named elements

 


four_four :: Meter 
four_four = TimeSig 4 4



