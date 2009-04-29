{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Mullein.AbcConvert
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Convert to Abc
--
--------------------------------------------------------------------------------


module Mullein.AbcConvert where

import Mullein.AbcNoteClass
import Mullein.CoreTypes
import Mullein.Duration
import Mullein.SpellingMap
import Mullein.Utils () -- State Applicative instance

import Control.Applicative
import Control.Monad.State
import Data.Ratio

data St = St { spelling_map :: SpellingMap, unit_note_length :: Duration }


type CM a = State St a



convertToAbc :: AbcNote e => SpellingMap -> Duration -> PartP e -> PartP e
convertToAbc smap unl e = evalState (cPart e) s0 where
    s0 = St  {spelling_map=smap, unit_note_length=unl} 

cPart :: AbcNote e => PartP e -> CM (PartP e)
cPart (Part as)           = Part <$> mapM cPhrase as


cPhrase :: AbcNote e => PhraseP e -> CM (PhraseP e)
cPhrase (Phrase a)        = Phrase <$> cMotif a
cPhrase (Repeated a)      = Repeated <$> cMotif a
cPhrase (FSRepeat a x y)  = FSRepeat <$> cMotif a <*> cMotif x <*> cMotif y


cMotif :: AbcNote e => MotifP e -> CM (MotifP e)
cMotif (Motif k m as)       = Motif k m <$> mapM cBar as


cBar :: AbcNote e => BarP e -> CM (BarP e)
cBar (Bar a)              = Bar <$> cUnison a 
cBar (Overlay a as)       = Overlay <$> cUnison a <*> mapM cUnison as

cUnison :: AbcNote e => UnisonP e -> CM (UnisonP e)
cUnison (Unison as tied)  = (\xs -> Unison xs tied) 
                              <$> mapM cBracket as

cBracket :: AbcNote e => BracketP e -> CM (BracketP e)
cBracket (Singleton a)    = Singleton <$> cElement a
cBracket (Bracket as)     = Bracket   <$> mapM cElement as


cElement :: AbcNote e => ElementP e -> CM (ElementP e)
cElement (Note p d)       = (\smap unl -> 
                             Note (respell smap p) (unitRescale unl d))
                              <$> gets spelling_map <*> gets unit_note_length
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

