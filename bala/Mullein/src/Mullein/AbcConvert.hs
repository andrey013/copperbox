{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Mullein.AbcConvert
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Convert to Abc
--
--------------------------------------------------------------------------------


module Mullein.AbcConvert where

import Mullein.CoreTypes
import Mullein.Duration
import Mullein.LabelSet
import Mullein.Pitch
import Mullein.RS

import Control.Applicative
import Data.Ratio

data St = St { label_set :: LabelSet, unit_note_length :: Duration }
data Env = Env {} 


type CM a = RS St Env a

convertToAbc :: LabelSet -> Duration -> PartP Pitch -> PartP Pitch
convertToAbc lset unl e = evalRS (cPart e) s0 e0 where
    s0 = St  {label_set=lset, unit_note_length=unl} 
    e0 = Env

cPart :: PartP Pitch -> CM (PartP Pitch)
cPart (Part as)           = Part <$> mapM cPhrase as


cPhrase :: PhraseP Pitch -> CM (PhraseP Pitch)
cPhrase (Phrase a)        = Phrase <$> cMotif a
cPhrase (Repeated a)      = Repeated <$> cMotif a
cPhrase (FSRepeat a x y)  = FSRepeat <$> cMotif a <*> cMotif x <*> cMotif y


cMotif :: MotifP Pitch -> CM (MotifP Pitch)
cMotif (Motif k m as)       = Motif k m <$> mapM cBar as


cBar :: BarP Pitch -> CM (BarP Pitch)
cBar (Bar a)              = Bar <$> cUnison a 
cBar (Overlay a as)       = Overlay <$> cUnison a <*> mapM cUnison as

cUnison :: UnisonP Pitch -> CM (UnisonP Pitch)
cUnison (Unison as tied)  = (\xs -> Unison xs tied) 
                              <$> mapM cBracket as

cBracket :: BracketP Pitch -> CM (BracketP Pitch)
cBracket (Singleton a)    = Singleton <$> cElement a
cBracket (Bracket as)     = Bracket   <$> mapM cElement as


cElement :: ElementP Pitch -> CM (ElementP Pitch)
cElement (Note p d)       = (\lset unl -> 
                             Note (naturalize lset p) (unitRescale unl d))
                              <$> gets label_set <*> gets unit_note_length
cElement (Rest d)         = (\unl -> Rest $ unitRescale unl d)
                              <$> gets unit_note_length
cElement (Spacer d)       = (\unl -> Spacer $ unitRescale unl d)
                              <$> gets unit_note_length
cElement (Chord _ _)      = error "A.Chord"
cElement (GraceNotes _)   = error "A.GraceNotes"



--------------------------------------------------------------------------------
-- helpers

unitRescale :: Duration -> Duration -> Duration
unitRescale unl drn = (dn%dd) / (un%ud) where
    (dn,dd)  = (numerator drn, denominator drn)
    (un,ud)  = (numerator unl, denominator unl)

