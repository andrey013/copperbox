{-# LANGUAGE FlexibleInstances       #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Mullein.LilyPondConvert
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
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
import Mullein.LilyPondNoteClass
import Mullein.Pitch
import Mullein.Utils () -- get State Applicative instance

import Control.Applicative
import Control.Monad.State
import Data.Ratio

data St = St { relative_pitch :: Maybe Pitch, relative_duration :: Duration }


type CM a = State St a

instance ExchangePitch (State St) where
  exchangePitch = exchPitch



convertToLyRelative :: LyNote e => Pitch -> PartP e -> PartP e
convertToLyRelative rp = convert (Just $ rescale rp)

convertToLyAbsolute :: LyNote e => PartP e -> PartP e
convertToLyAbsolute = convert Nothing

rescale :: Pitch -> Pitch
rescale p = rescaleOctave (-4) p

convert :: LyNote e => Maybe Pitch -> PartP e -> PartP e
convert mb_rp e = evalState (cPart e) s0 where
    s0 = St  {relative_pitch=mb_rp, relative_duration=1%4}

cPart :: LyNote e => PartP e -> CM (PartP e)
cPart (Part as)           = Part <$> mapM cPhrase as


cPhrase :: LyNote e => PhraseP e -> CM (PhraseP e)
cPhrase (Phrase a)        = Phrase <$> cMotif a
cPhrase (Repeated a)      = Repeated <$> cMotif a
cPhrase (FSRepeat a x y)  = FSRepeat <$> cMotif a <*> cMotif x <*> cMotif y


cMotif :: LyNote e => MotifP e -> CM (MotifP e)
cMotif (Motif k m as)       = Motif k m <$> mapM cBar as


-- Reset the duration at the start of the bar.
-- Although not strictly necessary, this makes deducing the duration
-- of a note in a the midst of generated score easier.
 
cBar :: LyNote e => BarP e -> CM (BarP e)
cBar (Bar a)              = Bar <$> (resetDuration *> cUnison a)
cBar (Overlay a as)       = Overlay <$> (resetDuration *> cUnison a) 
                                    <*> mapM cUnison as

cUnison :: LyNote e => UnisonP e -> CM (UnisonP e)
cUnison (Unison as tied)  = (\xs -> Unison xs tied) 
                              <$> mapM cBracket as

cBracket :: LyNote e => BracketP e -> CM (BracketP e)
cBracket (Singleton a)    = Singleton <$> cElement a
cBracket (Bracket as)     = Bracket   <$> mapM cElement as


cElement :: LyNote e => ElementP e -> CM (ElementP e)
cElement (Note p d)       = (\p' rd -> Note p' (relativeDuration rd d))
                              <$> rewritePitch p <*> exchDuration d
cElement (Rest d)         = (\rd -> Rest $ relativeDuration rd d)
                              <$> exchDuration d
cElement (Spacer d)       = (\rd -> Spacer $ relativeDuration rd d)
                              <$> exchDuration d
cElement (Chord ps d)     = (\ps' rd -> Chord ps' (relativeDuration rd d))
                              <$> mapM rewritePitch ps <*> exchDuration d
cElement (GraceNotes xs)  = GraceNotes <$> mapM cGraceNote xs

cGraceNote :: LyNote e => GraceNoteP e -> CM (GraceNoteP e)
cGraceNote (e,d)          = (\e' rd -> (e', relativeDuration rd d))
                              <$> rewritePitch e <*> exchDuration d


--------------------------------------------------------------------------------
-- helpers

exchDuration :: Duration -> CM Duration
exchDuration new = do
    old <- gets relative_duration
    modify $ \s -> s {relative_duration=new}
    return old

exchPitch :: Pitch -> CM Pitch
exchPitch new = do
    mb_old <- gets relative_pitch
    maybe (return new) justCont mb_old
  where
    justCont old = do { modify $ \s -> s {relative_pitch=Just new}
                      ; return old }


resetDuration :: CM ()
resetDuration = modify $ \s -> s {relative_duration=(-1)}


relativeDuration :: Duration -> Duration -> Duration
relativeDuration old drn | old == drn = -1
                         | otherwise = drn



-- LilyPond middle c is c' (aka `c 1`)
-- Mullein middle c is c5
absPitch :: Pitch -> Pitch -> Pitch
absPitch _ (Pitch l a o) = Pitch l a (o-4)


relPitch :: Pitch -> Pitch -> Pitch
relPitch old pch@(Pitch l a _) = Pitch l a (octaveDist old pch)
