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

-- import qualified Mullein.AbcSyntax as A
import Mullein.Duration
import Mullein.LabelSet
import Mullein.Pitch
import Mullein.RS
import Mullein.ScoreDatatypes

import Control.Applicative
import Data.Ratio

data St = St { label_set :: LabelSet, unit_note_length :: Duration }
data Env = Env {} 


type CM a = RS St Env a

convertToAbc :: LabelSet -> Duration -> Part Pitch -> Part Pitch
convertToAbc lset unl e = evalRS (cPart e) s0 e0 where
    s0 = St  {label_set=lset, unit_note_length=unl} 
    e0 = Env

cPart :: Part Pitch -> CM (Part Pitch)
cPart (Part as)           = Part <$> mapM cPhrase as


cPhrase :: Phrase Pitch -> CM (Phrase Pitch)
cPhrase (Phrase a)        = Phrase <$> cMotif a
cPhrase (Repeated a)      = Repeated <$> cMotif a
cPhrase (FSRepeat a x y)  = FSRepeat <$> cMotif a <*> cMotif x <*> cMotif y


cMotif :: Motif Pitch -> CM (Motif Pitch)
cMotif (Motif k m as)       = Motif k m <$> mapM cBar as


cBar :: Bar Pitch -> CM (Bar Pitch)
cBar (Bar a)              = Bar <$> cUnison a 
cBar (Overlay a as)       = Overlay <$> cUnison a <*> mapM cUnison as

cUnison :: Unison Pitch -> CM (Unison Pitch)
cUnison (Unison as tied)  = (\xs -> Unison xs tied) 
                              <$> mapM cBracket as

cBracket :: Bracket Pitch -> CM (Bracket Pitch)
cBracket (Singleton a)    = Singleton <$> cElement a
cBracket (Bracket as)     = Bracket   <$> mapM cElement as


cElement :: Element Pitch -> CM (Element Pitch)
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

