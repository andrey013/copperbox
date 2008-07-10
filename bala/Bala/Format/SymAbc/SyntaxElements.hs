{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts #-}


--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Format.SymAbcPond.SyntaxElements
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

module Bala.Format.SymAbc.SyntaxElements where

import Bala.Format.Base.SymBase
import Bala.Format.SymAbc.Datatypes

--------------------------------------------------------------------------------
-- * Contexts for lists

fieldCtx :: (CSnocList repr CT_Field) => repr (SnocList CT_Field)
fieldCtx = snil

elementCtx :: (CSnocList repr CT_Element) => repr (SnocList CT_Element)
elementCtx = snil


--------------------------------------------------------------------------------


-- | @l_field@ - alias for the very long name default_note_length_field.
l_field :: (CMidTuneField repr) => MeterFraction -> repr MidTuneField
l_field = default_note_length_field
  

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


-- rests 
z1, z2 :: (CRest repr, CDuration repr, CAttr repr) => repr Rest
z1 = rest `attr` dur 1
z2 = rest `attr` dur 2


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



firstRepeat, secondRepeat, firstEnding, secondEnding 
    :: (CRepeatMark repr) => repr RepeatMark
firstRepeat   = repeatMark "[1"
secondRepeat  = repeatMark "[2"
firstEnding   = repeatMark "|1"
secondEnding  = repeatMark ":|2"

