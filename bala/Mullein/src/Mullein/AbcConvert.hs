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

import qualified Mullein.AbcSyntax as A
import Mullein.Duration
import Mullein.LabelSet
import Mullein.RS
import Mullein.ScoreSyntax

import Control.Applicative
import Data.Ratio

data S = S { label_set :: LabelSet, unit_note_length :: Duration }
data E = E 


type CM a = RS S E a

convertToAbc :: LabelSet -> Duration -> Part Element -> Part A.Element
convertToAbc lset unl e = evalRS (cPart e) s0 e0 where
    s0 = S lset unl 
    e0 = E

cPart :: Part Element -> CM (Part A.Element)
cPart (Part as)           = Part <$> mapM cPhrase as


cPhrase :: Phrase Element -> CM (Phrase A.Element)
cPhrase (Phrase a)        = Phrase <$> cMotif a
cPhrase (Repeated a)      = Repeated <$> cMotif a
cPhrase (FSRepeat a x y)  = FSRepeat <$> cMotif a <*> cMotif x <*> cMotif y


cMotif :: Motif Element -> CM (Motif A.Element)
cMotif (Motif k m as)       = Motif k m <$> mapM cBar as


cBar :: Bar Element -> CM (Bar A.Element)
cBar (Bar a)              = Bar <$> cUnison a 
cBar (Overlay a as)       = Overlay <$> cUnison a <*> mapM cUnison as

cUnison :: Unison Element -> CM (Unison A.Element)
cUnison (Unison as tied)  = (\xs -> Unison xs tied) 
                              <$> mapM cBracket as

cBracket :: Bracket Element -> CM (Bracket A.Element)
cBracket (Singleton a)    = Singleton <$> cElement a
cBracket (Bracket as)     = Bracket   <$> mapM cElement as


cElement :: Element -> CM A.Element
cElement (Note p d)       = (\lset unl -> 
                             A.Note (naturalize lset p) (unitRescale unl d))
                              <$> gets label_set <*> gets unit_note_length
cElement (Rest d)         = (\unl -> A.Rest $ unitRescale unl d)
                              <$> gets unit_note_length
cElement (Spacer d)       = (\unl -> A.Spacer $ unitRescale unl d)
                              <$> gets unit_note_length
cElement (Chord _ _)      = error "A.Chord"
cElement (GraceNotes _)   = error "A.GraceNotes"



--------------------------------------------------------------------------------
-- helpers

unitRescale :: Duration -> Duration -> Duration
unitRescale unl drn = (dn%dd) / (un%ud) where
    (dn,dd)  = (numerator drn, denominator drn)
    (un,ud)  = (numerator unl, denominator unl)

