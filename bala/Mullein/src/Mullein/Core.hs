{-# LANGUAGE MultiParamTypeClasses      #-}
{-# OPTIONS -Wall #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Mullein.Core
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Common functions operating on core types
--
--------------------------------------------------------------------------------

module Mullein.Core where

import Mullein.CoreTypes
import Mullein.Duration
import Mullein.Pitch
import Mullein.Gen.Syntax ( Element(..) )
import Mullein.Utils

import Control.Monad.State
import Data.List ( elemIndex, find )
import qualified Data.Map as Map
import Data.Ratio


--------------------------------------------------------------------------------
-- Note lists

type NoteCtx a = State NoteListCtx a

-- NoteListCtx represents /shorthand state/ so we can omit
-- some details when building the notelist (e.g. duration) 
data NoteListCtx = NoteListCtx 
      { unit_note_length :: Duration }
  deriving (Eq,Show)


notelist :: [NoteCtx Element] -> [Element]
notelist fs = evalState (sequence fs) ctx0 where
    ctx0 = NoteListCtx { unit_note_length = 1%4 }

(&) :: NoteCtx Element -> NoteCtx () -> NoteCtx Element
(&) f upd  = upd >> f 

-- Building overlays

type BarNum = Int
type OverlayList = (NoteList, [(BarNum,NoteList)])

primary :: NoteList -> OverlayList
primary xs = (xs,[])

addOverlay :: BarNum -> NoteList -> OverlayList -> OverlayList
addOverlay n xs (p,xss) = (p,(n,xs):xss)




--------------------------------------------------------------------------------
-- Musical representation




--------------------------------------------------------------------------------
-- Meter

meterFraction :: Meter -> Duration
meterFraction (TimeSig n d) = n%d
meterFraction CommonTime    = 4%4 
meterFraction CutTime       = 2%2 
           

metricalSpec :: Int -> Int -> MetricalSpec
metricalSpec n d 
      | compoundMeter  n d  = (time_sig, replicate 3 $ (rational n d) / 3)
      | simpleMeter n d     = (time_sig, replicate n (rational 1 d))
      | otherwise           = error $ err_msg
  where
    time_sig = TimeSig (fromIntegral n) (fromIntegral d)
 
    err_msg = "simpleMetricalSpec - can't generate a meter pattern for a "
           ++ "meter that is neither simple or compound."



-- Note compoundMeter and simpleMeter overlap

compoundMeter :: Integral a => a -> a -> Bool
compoundMeter n d = log2whole d && (n `mod` 3 == 0)
         
simpleMeter :: Integral a => a -> a -> Bool
simpleMeter _ d = log2whole d

log2whole :: Integral a => a -> Bool
log2whole = (==0) . snd . pf . logBase 2 . fromIntegral where
    pf :: Double -> (Int, Double)
    pf = properFraction
        
--------------------------------------------------------------------------------
-- pitch labels        

-- Cancel the accidental if the pitch is found in the label set
-- This is the transformation needed for Abc: 
-- f# should be printed f in g major
naturalize :: LabelSet -> Pitch -> Pitch
naturalize lbls p = maybe p nat (labelSetFind p lbls) where
    nat (Pitch l _ o) = Pitch l Nat o

labelSet :: [PitchLabel] -> LabelSet
labelSet = LabelSet . foldl fn Map.empty
  where fn m p = Map.insert (semitones p) p m    

labelSetFind :: Pitch -> LabelSet -> Maybe Pitch
labelSetFind (Pitch l a o) (LabelSet m) = 
    maybe Nothing (fn o) (Map.lookup (semitones l + semitones a) m) 
  where
    fn ove (PitchLabel ltr atl) = Just $ Pitch ltr atl ove

labelSetOf :: Key -> Maybe LabelSet
labelSetOf (Key (PitchLabel l a) m xs)  = scaleSpelling l a m xs

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


order_of_sharps :: [PitchLetter]
order_of_sharps = [F,C,G,D,A,E,B]

order_of_flats :: [PitchLetter]
order_of_flats = reverse order_of_sharps

twist :: Eq a => (a -> b) -> (a -> b) -> [a] -> [a] -> [b]
twist sk fk ys ss = foldr step [] ss
  where
    step a acc | a `elem` ys = (sk a):acc
               | otherwise   = (fk a):acc      