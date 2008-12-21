{-# LANGUAGE MultiParamTypeClasses      #-} 
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.MusicRep.GuitarNotation
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- LilyPond guitar output...
--
--------------------------------------------------------------------------------


module Bala.MusicRep.GuitarNotation where

import Bala.Base.BaseExtra
import Bala.Base.Pitch

import Data.List
 
-- e.g. \fret-diagram-terse #"7-1-(;9-3;7-1-);8-2;x;x;"

data TerseFretDiagram = TerseFretDiagram { 
        _string_six     :: Fingering,
        _string_five    :: Fingering,
        _string_four    :: Fingering,
        _string_three   :: Fingering,
        _string_two     :: Fingering,
        _string_one     :: Fingering
    }
  deriving (Show)

  
data Fingering = Fretted { _fret        :: Int, 
                           _opt_finger  :: Maybe Int,
                           _barre       :: Maybe Barre } 
               | OpenString        
               | Omit 
  deriving (Eq,Show)


data Barre = BarreStart | BarreEnd
  deriving (Enum,Eq,Ord,Show)

data GuitarTuning = GuitarTuning { 
      _tstring_six    :: Pitch,
      _tstring_five   :: Pitch,
      _tstring_four   :: Pitch,
      _tstring_three  :: Pitch,
      _tstring_two    :: Pitch,
      _tstring_one    :: Pitch
    }
  deriving (Show)

instance Listify TerseFretDiagram Fingering where
  listify (TerseFretDiagram s6 s5 s4 s3 s2 s1) = [s6,s5,s4,s3,s2,s1]
  
  
instance Listify GuitarTuning Pitch where
  listify (GuitarTuning s6 s5 s4 s3 s2 s1) = [s6,s5,s4,s3,s2,s1]
  
newtype ST_Chord = ST_Chord { getST_Chord :: TerseFretDiagram }


instance PitchValue ST_Chord where
  pitchValue (ST_Chord ch) = pcat ch standard_tuning
  
  updatePitch _ _ = error "ST_Chord - modifyPitch not implemented"

  
stChord :: Fingering -> Fingering -> Fingering ->
           Fingering -> Fingering -> Fingering -> ST_Chord
stChord s6 s5 s4 s3 s2 s1 = ST_Chord $ TerseFretDiagram s6 s5 s4 s3 s2 s1             

standard_tuning :: GuitarTuning
standard_tuning = GuitarTuning e3 a3 d4 g4 b4 e5     

-- pitches for chord and tuning 
pcat :: TerseFretDiagram -> GuitarTuning -> [Pitch]
pcat ch tn = step $ zip (listify ch) (listify tn) where
  step []             = []
  step ((Fretted i _ _, p) : xs) = p `increase` i : step xs
  step ((OpenString   , p) : xs) = p : step xs
  step ((Omit         , p) : xs) = step xs
--------------------------------------------------------------------------------
-- Building...
  
x :: Fingering
x = Omit

o :: Fingering
o = OpenString

ff :: Int -> Int -> Fingering
ff ft fgr = Fretted ft (Just fgr) Nothing

ffbs :: Int -> Int -> Fingering
ffbs ft fgr = Fretted ft (Just fgr) (Just BarreStart)

ffbe :: Int -> Int -> Fingering
ffbe ft fgr = Fretted ft (Just fgr) (Just BarreEnd)


--------------------------------------------------------------------------------
-- Printing...

fdiagram :: TerseFretDiagram -> String
fdiagram = ($ "") . tfdS

tfdS :: TerseFretDiagram -> ShowS
tfdS =  trailSepS semiS . map fgrS . listify


-- fgrS - showsFingering needs a short name
fgrS :: Fingering -> ShowS
fgrS (Fretted ft ofgr obarre)   = shows ft . suffixfgr . suffixbarre where
    suffixfgr   = maybe id ((.) dashS . shows) ofgr
    suffixbarre = maybe id ((.) dashS . barreS) obarre
fgrS OpenString                 = showChar 'o'          
fgrS Omit                       = showChar 'x'


barreS :: Barre -> ShowS
barreS BarreStart = showChar '('
barreS BarreEnd   = showChar ')'


sepS :: ShowS -> [ShowS] -> ShowS
sepS sep []      = id
sepS sep (x:xs)  = foldl' (\a e -> a . sep . e) x xs

trailSepS :: ShowS -> [ShowS] -> ShowS
trailSepS sep xs      = foldr (\e a -> e . sep . a) id xs

 
semiS       :: ShowS
semiS       = showChar ';'

dashS       :: ShowS
dashS       = showChar '-'
