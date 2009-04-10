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
import Mullein.Syntax ( Section )
import Mullein.Utils

import Data.List ( elemIndex, find )
import qualified Data.Map as Map
import Data.Ratio
import Data.Sequence ( (|>) )
import qualified Data.Sequence as S



--------------------------------------------------------------------------------
-- Note lists




note :: Pitch -> Duration -> NoteList -> NoteList
note p d t = t |> (Note p d)

rest :: Duration -> NoteList -> NoteList
rest d t = t |> (Rest d)

root :: NoteList
root = S.empty


--------------------------------------------------------------------------------
-- structured /sections/.



--------------------------------------------------------------------------------
-- aggregate sections


--------------------------------------------------------------------------------
-- aggregate sections

data Aggregate a = Aggregate a :>> Aggregate a
                 | Literal (Section a)
                 | Repeated (Section a)                 
                 | AltRepeat { body, end1, end2 :: Section a }
                 | KeyChange Key 



-- Do automatic coercion on snoc-ing...
class Snoc c c' where
  (|>>) :: c a -> c' a -> Aggregate a

instance Snoc Section Section where
  (|>>) a b = Literal a :>> Literal b
  
instance Snoc Aggregate Aggregate where
  (|>>) a b = a :>> b
  
instance Snoc Section Aggregate where
  (|>>) a b = Literal a :>> b
    
instance Snoc Aggregate Section where
  (|>>) a b = a :>> Literal b
  
repeated :: Section a -> Aggregate a
repeated = Repeated

keyChange :: Key -> Aggregate a
keyChange = KeyChange



--------------------------------------------------------------------------------
-- Musical representation




--------------------------------------------------------------------------------
-- Meter

meterFraction :: Meter -> Duration
meterFraction (TimeSig n d) = n%d
meterFraction CommonTime    = 4%4 
meterFraction CutTime       = 2%2 
           

metricalSpec :: Integral a => a -> a -> MetricalSpec
metricalSpec n d 
      | compoundMeter  n d  = (time_sig, replicate 3 $ (rational n d) / 3)
      | simpleMeter n d     = (time_sig, replicate (fromIntegral n) (rational 1 d))
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
naturalize lbls p = maybe p ((flip accidentalConst) Nat) (labelSetFind p lbls)

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