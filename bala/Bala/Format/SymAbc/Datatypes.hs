{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Format.SymAbc.Datatypes
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Datatypes for ABC format
--
--------------------------------------------------------------------------------

module Bala.Format.SymAbc.Datatypes  where

import Data.Ratio

data Concatenation ctx

infixl 5 +++
class SymConcatenation ctx repr where
  (+++)  :: repr (a ctx) -> repr (b ctx) -> repr (Concatenation ctx)
 
    
data Ctx_Field
 
data Ctx_KeyField
  
-- | notes, rest, slurs, barlines...
data Ctx_Element 





data Field ctx
class SymField repr where
  field :: Char -> repr a -> repr (Field Ctx_Field)
  

class SynNumberField repr where
  num_                :: Int -> repr (Field Ctx_Field)        -- 'X'

class SymTextFields repr where
  area_               :: String -> repr (Field Ctx_Field)     -- 'A'
  book_               :: String -> repr (Field Ctx_Field)     -- 'B'
  composer_           :: String -> repr (Field Ctx_Field)     -- 'C' 
  discography_        :: String -> repr (Field Ctx_Field)     -- 'D'
  elemskip_           :: String -> repr (Field Ctx_Field)     -- 'E'
  group_              :: String -> repr (Field Ctx_Field)     -- 'G'
  information_        :: String -> repr (Field Ctx_Field)     -- 'I'
  notes_              :: String -> repr (Field Ctx_Field)     -- 'N'
  origin_             :: String -> repr (Field Ctx_Field)     -- 'O'
  rhythm_             :: String -> repr (Field Ctx_Field)     -- 'R'
  source_             :: String -> repr (Field Ctx_Field)     -- 'S'
  text_               :: String -> repr (Field Ctx_Field)     -- 'T'
  words_              :: String -> repr (Field Ctx_Field)     -- 'W'
  transcriberNotes_   :: String -> repr (Field Ctx_Field)     -- 'Z'

class SymHistoryField repr where
  history_       :: [String] -> repr (Field Ctx_Field)         -- 'H'


class SymKeyField repr where
  key_                :: repr (Key ctx) -> repr (Field Ctx_Field)         -- 'K'
  
 
class SymDefaultLengthField repr where
  defaultLength_      :: Rational -> repr (Field Ctx_Field)         -- 'L'
 
 
class SymTempoField repr where
  tempo_              :: repr (Tempo ctx) -> repr (Field Ctx_Field) -- 'Q'  

class SymMeterField repr where
  meter_              :: repr (Meter ctx) -> repr (Field Ctx_Field) -- 'M'  

data AbcMusic ctx
class SymAbcMusic repr where
  abcmusic         :: repr (a ctx) -> repr (AbcMusic Ctx_Field)
  
  
   
  
data Tempo ctx
class SymTempo repr where
  tempo               :: Int -> repr (Tempo ctx)
  ctempo              :: repr (Length ctx) -> Int -> repr (Tempo ctx)
  stempo              :: Rational -> Int -> repr (Tempo ctx)
  
data Length ctx
class SymLength repr where
  ilength             :: Int -> repr (Length ctx)
  flength             :: Rational -> repr (Length ctx) 

  
  
data Key ctx
class SymKey repr where
  key                 :: repr (KeySpec ctx) -> repr (Key ctx)
  highlandNoKey       :: repr (Key ctx)
  highlandMixolydian  :: repr (Key ctx)


data KeySpec ctx
class SymKeySpec repr where
  keySpec :: repr (BaseNote ctx) -> repr (KeySpec ctx)
      
  
data KeyAccidental ctx

class SymKeyAccidental repr where
  keySharp  :: repr (KeyAccidental ctx)
  keyFlat   :: repr (KeyAccidental ctx)
    

class AttrMode ctx
class SymAttrMode repr where
  mode :: (AttrMode a) => String -> repr (a ctx) -> repr (a ctx)
{-
locrian 
    ::  (AttrMode a, SymAttrMode repr) => repr (a ctx) -> repr (a ctx)   
locrian       = mode "loc"
-}

major, minor, lydian, ionian, mixolydian, dorian, aeolian, phrygian, locrian 
    ::  (AttrMode a, SymAttrMode repr) => repr (a ctx) -> repr (a ctx)   
major         = mode "maj"
minor         = mode "min"
lydian        = mode "lyd"
ionian        = mode "ion"
mixolydian    = mode "mix"
dorian        = mode "dor"
aeolian       = mode "aeo"
phrygian      = mode "phr"
locrian       = mode "loc"



instance AttrMode KeySpec



data Meter ctx
class SymMeter repr where
  meter      :: Rational -> repr (Meter ctx) 
  commonTime :: repr (Meter ctx)
  cutTime    :: repr (Meter ctx)

    

class AttrDuration ctx
class SymAttrDuration repr where
  dur :: (AttrDuration a) => Int -> repr (a ctx)  -> repr (a ctx)     

instance AttrDuration BaseNote
instance AttrDuration Rest



data Rest ctx
class SymRest repr where
  rest :: repr (Rest Ctx_Element)

class AttrOctave ctx
class SymAttrOctave repr where
  octaveLow     :: (AttrOctave a) => Int -> repr (a ctx) -> repr (a ctx) 
  octaveHigh    :: (AttrOctave a) => Int -> repr (a ctx) -> repr (a ctx) 
    
instance AttrOctave BaseNote


class AttrAccidental ctx
class SymAttrAccidental repr where 
  natural       :: (AttrAccidental a) => repr (a ctx) -> repr (a ctx)
  sharp         :: (AttrAccidental a) => repr (a ctx) -> repr (a ctx)
  doubleSharp   :: (AttrAccidental a) => repr (a ctx) -> repr (a ctx)
  flat          :: (AttrAccidental a) => repr (a ctx) -> repr (a ctx)
  doubleFlat    :: (AttrAccidental a) => repr (a ctx) -> repr (a ctx)



instance AttrAccidental BaseNote


-- Abc has pitches in a two octave range and then uses octave specs for higher
-- and lower octaves
data PitchLetter = C | D | E | F | G | A | B | C2 | D2 | E2 | F2 | G2 | A2 | B2
  deriving (Eq,Show) 

data BaseNote ctx
class SymBaseNote repr where
  note          :: PitchLetter -> repr (BaseNote ctx)



c_, d_, e_, f_, g_, a_, b_ :: (SymBaseNote repr) =>  repr (BaseNote ctx)
c_  = note C
d_  = note D
e_  = note E
f_  = note F
g_  = note G
a_  = note A
b_  = note B

c__, d__, e__, f__, g__, a__, b__ :: (SymBaseNote repr) =>  repr (BaseNote ctx)
c__  = note C2
d__  = note D2
e__  = note E2
f__  = note F2
g__  = note G2
a__  = note A2
b__  = note B2


data BrokenRhythm ctx
class SymBrokenRhythm repr where
  -- '>' left note dotted, right note halved
  dottedLeft    :: Int -> repr (BrokenRhythm ctx)
    
  -- '<' left note halved, right note dotted 
  dottedRight   :: Int -> repr (BrokenRhythm ctx)   
  

  
data Tie ctx
class SymTie repr where
  tie         :: repr (Tie ctx)

  
      
data Grace ctx
class SymGrace repr where
    tilde       :: repr (Grace ctx)
    stacatto    :: repr (Grace ctx)
    downBow     :: repr (Grace ctx)
    upBow       :: repr (Grace ctx)
    
    
        
data RepeatMark ctx
class SymRepeatMark repr where
  repeatMark :: String -> repr (RepeatMark Ctx_Element)

firstRepeat, secondRepeat, firstEnding, secondEnding 
    :: (SymRepeatMark repr) => repr (RepeatMark Ctx_Element)
firstRepeat   = repeatMark "[1"
secondRepeat  = repeatMark "[2"
firstEnding   = repeatMark "|1"
secondEnding  = repeatMark ":|2"

 

data Slur ctx
class SymSlur repr where
  beginSlur :: repr (Slur Ctx_Element)
  endSlur   :: repr (Slur Ctx_Element)
  
  
  
data TexCommand ctx
class SymTexComamnd repr where
  texCommand :: String -> repr (TexCommand ctx)
  
  