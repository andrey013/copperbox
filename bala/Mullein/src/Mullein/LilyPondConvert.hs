{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Mullein.LilyPondConvert
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Convert to LilyPond
--
--------------------------------------------------------------------------------


module Mullein.LilyPondConvert where


import Mullein.CoreTypes
import Mullein.Duration
import Mullein.Pitch
import Mullein.RS

import Control.Applicative
import Data.Ratio

data St = St { relative_pitch :: Pitch, relative_duration :: Duration }
data Env = Env { pitchConvert :: Pitch -> Pitch -> Pitch }


type CM a = RS St Env a

convertToLy :: (Pitch -> Pitch -> Pitch) -> Pitch -> PartP Pitch -> PartP Pitch
convertToLy f rp e = evalRS (cPart e) s0 e0 where
    s0 = St  {relative_pitch=rp, relative_duration=1%4}
    e0 = Env {pitchConvert=f} 

cPart :: PartP Pitch -> CM (PartP Pitch)
cPart (Part as)           = Part <$> mapM cPhrase as


cPhrase :: PhraseP Pitch -> CM (PhraseP Pitch)
cPhrase (Phrase a)        = Phrase <$> cMotif a
cPhrase (Repeated a)      = Repeated <$> cMotif a
cPhrase (FSRepeat a x y)  = FSRepeat <$> cMotif a <*> cMotif x <*> cMotif y


cMotif :: MotifP Pitch -> CM (MotifP Pitch)
cMotif (Motif k m as)       = Motif k m <$> mapM cBar as


-- Reset the duration at the start of the bar.
-- Although not strictly necessary, this makes deducing the duration
-- of a note in a the midst of generated score easier.
 
cBar :: BarP Pitch -> CM (BarP Pitch)
cBar (Bar a)              = Bar <$> (resetDuration *> cUnison a)
cBar (Overlay a as)       = Overlay <$> (resetDuration *> cUnison a) 
                                    <*> mapM cUnison as

cUnison :: UnisonP Pitch -> CM (UnisonP Pitch)
cUnison (Unison as tied)  = (\xs -> Unison xs tied) 
                              <$> mapM cBracket as

cBracket :: BracketP Pitch -> CM (BracketP Pitch)
cBracket (Singleton a)    = Singleton <$> cElement a
cBracket (Bracket as)     = Bracket   <$> mapM cElement as


cElement :: ElementP Pitch -> CM (ElementP Pitch)
cElement (Note p d)       = (\f rp rd -> 
                              Note (f rp p) (relativeDuration rd d))
                              <$> asks pitchConvert <*> exchPitch p 
                                                    <*> exchDuration d
cElement (Rest d)         = (\rd -> Rest $ relativeDuration rd d)
                              <$> exchDuration d
cElement (Spacer d)       = (\rd -> Spacer $ relativeDuration rd d)
                              <$> exchDuration d
cElement (Chord _ _)      = error "Chord"
cElement (GraceNotes _)   = error "GraceNotes"





--------------------------------------------------------------------------------
-- helpers

exchDuration :: Duration -> CM Duration
exchDuration new = do
    old <- gets relative_duration
    modify $ \s -> s {relative_duration=new}
    return old

exchPitch :: Pitch -> CM Pitch
exchPitch new = do
    old <- gets relative_pitch
    modify $ \s -> s {relative_pitch=new}
    return old


resetDuration :: CM ()
resetDuration = modify $ \s -> s {relative_duration=(-1)}


relativeDuration :: Duration -> Duration -> Duration
relativeDuration ud drn | ud == drn = -1
                        | otherwise = drn


-- LilyPond middle c is c' (aka `c 1`)
-- Mullein middle c is c4
absPitch :: Pitch -> Pitch -> Pitch
absPitch _ (Pitch l a o) = Pitch l a (o-3)


relPitch :: Pitch -> Pitch -> Pitch
relPitch rel pch@(Pitch l a _) = Pitch l a (pch `octaveDist` rel)
