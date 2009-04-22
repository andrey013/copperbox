{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Mullein.LabelSet
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined
--
-- Label Sets - the set of pitch names in a scale.
-- Used by Abc for note names.
--
--------------------------------------------------------------------------------

module Mullein.LabelSet (
  LabelSet,
  naturalize,
  makeLabelSet,
  labelSet,
  labels,
  trebleKeyMarks

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


newtype LabelSet = LabelSet { getLabelSet :: Map.Map Int PitchLabel }
  deriving (Show)


-- Cancel the accidental if the pitch is found in the label set
-- This is the transformation needed for Abc: 
-- f# should be printed f in g major
naturalize :: LabelSet -> Pitch -> Pitch
naturalize lbls p = maybe p nat (labelSetFind p lbls) where
    nat (Pitch l _ o) = Pitch l Nat o

-- Make a label set for the given key.
-- If the is irregular (i.e. not in the table above) then the function 
-- returns Nothing.
makeLabelSet :: Key -> Maybe LabelSet
makeLabelSet (Key (PitchLabel l a) m xs)
    = case elemIndex (l,a) $ modeLabels m of
        Just i  -> Just $ mkLS (7 - i) l xs
        Nothing -> Nothing


-- Create a label set /by hand/.
labelSet :: [PitchLabel] -> LabelSet
labelSet = LabelSet . foldl fn Map.empty
  where fn m p = Map.insert (semitones p) p m    

labelSetFind :: Pitch -> LabelSet -> Maybe Pitch
labelSetFind (Pitch l a o) (LabelSet m) = 
    maybe Nothing (fn o) (Map.lookup (semitones l + semitones a) m) 
  where
    fn ove (PitchLabel ltr atl) = Just $ Pitch ltr atl ove


labels :: LabelSet -> [PitchLabel]
labels = Map.elems . getLabelSet

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

mkLS :: Int -> PitchLetter -> [PitchLabel] -> LabelSet
mkLS i letter accidentals 
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


order_of_sharps :: [PitchLetter]
order_of_sharps = [F,C,G,D,A,E,B]

order_of_flats :: [PitchLetter]
order_of_flats = reverse order_of_sharps

twist :: Eq a => (a -> b) -> (a -> b) -> [a] -> [a] -> [b]
twist sk fk ys ss = foldr step [] ss
  where
    step a acc | a `elem` ys = (sk a):acc
               | otherwise   = (fk a):acc      

--------------------------------------------------------------------------------
-- manual key signatures for LilyPond

type Octave = Int
type Step = Int

type ManualKeyMark = (Octave,Step,Accidental)



trebleKeyMarks :: Key -> [ManualKeyMark]
trebleKeyMarks key@(Key _ _ xs) = step $ numAccidentals key where
    step i | i < 0     = addExtras xs $ take (abs i) treble_flats 
           | otherwise = addExtras xs $ take i treble_sharps
    
    addExtras _ ys = ys -- TODO


treble_sharps :: [ManualKeyMark]
treble_sharps = 
    [ (oPosTs F,3,Sharp)
    , (oPosTs C,0,Sharp)
    , (oPosTs G,4,Sharp)
    , (oPosTs D,1,Sharp)
    , (oPosTs A,5,Sharp)
    , (oPosTs E,2,Sharp)
    , (oPosTs B,6,Sharp)
    ]
    
treble_flats :: [ManualKeyMark]
treble_flats =
    [ (oPosTf B,6,Flat)
    , (oPosTf E,2,Flat)
    , (oPosTf A,5,Flat)
    , (oPosTf D,1,Flat)
    , (oPosTf G,4,Flat)
    , (oPosTf C,0,Flat)
    , (oPosTf F,3,Flat)
    ]


-- negative represents flats - (-3) is 3 flats
numAccidentals :: Key -> Int 
numAccidentals (Key (PitchLabel l a) m _) = 
    rot m $ acc a $ pos l 
  where

    pos :: PitchLetter -> Int
    pos C = 0 
    pos G = 1
    pos D = 2
    pos A = 3
    pos E = 4
    pos B = 5
    pos F = (-1)
    
    acc :: Accidental -> (Int-> Int)
    acc Nat   = id
    acc Sharp = (+ 7)
    acc Flat  = flip (-) 7
    acc _     = error "key signature is not normalized"

    rot :: Mode -> (Int -> Int) 
    rot Major       = (+ 0)
    rot Minor       = flip (-) 3
    rot Lydian      = flip (-) 1
    rot Ionian      = rot Major
    rot Mixolydian  = flip (-) 1
    rot Dorian      = flip (-) 2
    rot Aeolian     = rot Minor
    rot Phrygian    = flip (-) 4
    rot Locrian     = flip (-) 5


-- octavePosition is how many octaves above middle C the (altered) pitch is 
-- printed in a key signature, e.g. F# is printed through line 5 - the top 
-- line of the staff, one octave above middle c. B# is printed through line
-- 3, in the same octave as middle C

-- octave position of sharps on treble clef
oPosTs :: PitchLetter -> Int
oPosTs C = 1
oPosTs D = 1
oPosTs E = 1
oPosTs F = 1
oPosTs G = 1
oPosTs A = 0
oPosTs B = 0

-- octave position of flats on treble clef
oPosTf :: PitchLetter -> Int
oPosTf C = 1
oPosTf D = 1
oPosTf E = 1
oPosTf F = 0
oPosTf G = 0
oPosTf A = 0
oPosTf B = 0

