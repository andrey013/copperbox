{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts #-}


--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Format.SymAbc.SyntaxElements
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- A 'concrete' syntax built on the abstract syntax provided by the datatypes
--
--------------------------------------------------------------------------------

module Bala.Format.SymAbc.SyntaxElements (
  -- * Contexts for lists
  fieldCtx, elementCtx,

  -- * Information fields (3)  
  l_field,

  -- ** K: key (3.1.14)
  major, minor, lydian, ionian, mixolydian, 
  dorian, aeolian, phrygian, locrian, 
  
  -- * The tune body (4)
  tunebody_elements,

  -- ** Pitch (4.1)
  c_, d_, e_, f_, g_, a_, b_, 
  c__, d__, e__, f__, g__, a__, b__,
  
  -- ** Rests (4.5)
  z1, z2, z4, z'2,

  -- ** Repeat \/ bar symbols & First and second repeats (4.8 & 4.9)
  barline, thinThick, thickThin,
  beginRepeat, endRepeat,
  firstRepeat, secondRepeat, firstEnding, secondEnding 
   
  ) where

import Bala.Format.Base.SymBase
import Bala.Format.SymAbc.Datatypes
import Bala.Base.Meter

--------------------------------------------------------------------------------
-- * Contexts for lists

fieldCtx :: (CSnocList repr CT_Field) => repr (SnocList CT_Field)
fieldCtx = snil

elementCtx :: (CSnocList repr CT_Element) => repr (SnocList CT_Element)
elementCtx = snil


--------------------------------------------------------------------------------
-- * Information fields (3)

-- | @l_field@ - alias for the very long name default_note_length_field.
l_field :: (CMidTuneField repr) => MeterFraction -> repr MidTuneField
l_field = default_note_length_field


-- ** K: key (3.1.14)
major, minor, lydian, ionian, mixolydian, dorian, aeolian, phrygian, locrian 
    ::  (CMode repr) => repr Mode 
major         = mode "maj"
minor         = mode "min"
lydian        = mode "lyd"
ionian        = mode "ion"
mixolydian    = mode "mix"
dorian        = mode "dor"
aeolian       = mode "aeo"
phrygian      = mode "phr"
locrian       = mode "loc"

-- * The tune body (4)

tunebody_elements :: (CSnocList repr CT_Element,
                      ListContext CT_Element a,
                      CTuneBody repr,
                      CAbcLine repr) 
                  => repr a -> repr TuneBody
tunebody_elements e = tunebody (elements (elementCtx +++ e))

-- ** Pitch (4.1)

c_, d_, e_, f_, g_, a_, b_ :: (CBaseNote repr) =>  repr BaseNote
c_  = note C
d_  = note D
e_  = note E
f_  = note F
g_  = note G
a_  = note A
b_  = note B

c__, d__, e__, f__, g__, a__, b__ :: (CBaseNote repr) =>  repr BaseNote
c__  = note C2
d__  = note D2
e__  = note E2
f__  = note F2
g__  = note G2
a__  = note A2
b__  = note B2


-- ** Rests (4.5)

-- @z1@ - a rest of the default note length.
z1        :: (CRest repr, CDuration repr, CAttr repr) => repr Rest
z1        = rest `attr` dur (1 // 1)

-- @z1@ - a rest of double the default note length.
z2        :: (CRest repr, CDuration repr, CAttr repr) => repr Rest
z2        = rest `attr` dur (2 // 1)

-- @z4@ - a rest four times the default note length.
z4        :: (CRest repr, CDuration repr, CAttr repr) => repr Rest
z4        = rest `attr` dur (2 // 1)

-- @z'2@ - a rest of half the default note length.
z'2       :: (CRest repr, CDuration repr, CAttr repr) => repr Rest
z'2       = rest `attr` dur (1 // 2)



-- ** Repeat \/ bar symbols & First and second repeats (4.8 & 4.9)

-- | @barline@ - single stroke @|@.
barline         :: (CRepeatMark repr) => repr RepeatMark
barline         = repeatMark "|" 

-- | @thinThick@ - @|]@.
thinThick       :: (CRepeatMark repr) => repr RepeatMark
thinThick       = repeatMark "|]" 


-- | @thickThin@ - @|]@.
thickThin       :: (CRepeatMark repr) => repr RepeatMark
thickThin       = repeatMark "[|" 

-- | @beginRepeat@ - @|:@.
beginRepeat     :: (CRepeatMark repr) => repr RepeatMark
beginRepeat     = repeatMark "|:"

-- | @endRepeat@ - @|:@.
endRepeat       :: (CRepeatMark repr) => repr RepeatMark
endRepeat       = repeatMark ":|"

-- | @doubleRepeat@ - @::@.
doubleRepeat    :: (CRepeatMark repr) => repr RepeatMark
doubleRepeat    = repeatMark "::"


-- | @firstRepeat@ - @[1@.
firstRepeat    :: (CRepeatMark repr) => repr RepeatMark
firstRepeat   = repeatMark "[1"

-- | @secondRepeat@ - @[2@.
secondRepeat    :: (CRepeatMark repr) => repr RepeatMark
secondRepeat  = repeatMark "[2"

-- | @firstEnding@ - @|1@.
firstEnding    :: (CRepeatMark repr) => repr RepeatMark
firstEnding   = repeatMark "|1"


-- | @secondEnding@ - @:|2@.
secondEnding    :: (CRepeatMark repr) => repr RepeatMark
secondEnding  = repeatMark ":|2"

