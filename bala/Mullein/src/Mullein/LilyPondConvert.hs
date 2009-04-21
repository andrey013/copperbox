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
import Mullein.Utils () -- get State Applicative instance

import Control.Applicative
import Control.Monad.State
import Data.Ratio

data St = St { relative_pitch :: Pitch, relative_duration :: Duration }


type CM a = State St a

convertToLy :: LyPitchC e => Pitch -> PartP e -> PartP e
convertToLy rp e = evalState (cPart e) s0 where
    s0 = St  {relative_pitch=rp, relative_duration=1%4}

class LyPitchC e where
  lyPitchC :: e -> CM e
  lyPitchesC :: [e] -> CM [e]  
    -- in LilyPond's relative mode chords have a special behaviour 
    -- that must be captured


cPart :: LyPitchC e => PartP e -> CM (PartP e)
cPart (Part as)           = Part <$> mapM cPhrase as


cPhrase :: LyPitchC e => PhraseP e -> CM (PhraseP e)
cPhrase (Phrase a)        = Phrase <$> cMotif a
cPhrase (Repeated a)      = Repeated <$> cMotif a
cPhrase (FSRepeat a x y)  = FSRepeat <$> cMotif a <*> cMotif x <*> cMotif y


cMotif :: LyPitchC e => MotifP e -> CM (MotifP e)
cMotif (Motif k m as)       = Motif k m <$> mapM cBar as


-- Reset the duration at the start of the bar.
-- Although not strictly necessary, this makes deducing the duration
-- of a note in a the midst of generated score easier.
 
cBar :: LyPitchC e => BarP e -> CM (BarP e)
cBar (Bar a)              = Bar <$> (resetDuration *> cUnison a)
cBar (Overlay a as)       = Overlay <$> (resetDuration *> cUnison a) 
                                    <*> mapM cUnison as

cUnison :: LyPitchC e => UnisonP e -> CM (UnisonP e)
cUnison (Unison as tied)  = (\xs -> Unison xs tied) 
                              <$> mapM cBracket as

cBracket :: LyPitchC e => BracketP e -> CM (BracketP e)
cBracket (Singleton a)    = Singleton <$> cElement a
cBracket (Bracket as)     = Bracket   <$> mapM cElement as


cElement :: LyPitchC e => ElementP e -> CM (ElementP e)
cElement (Note p d)       = (\p' rd -> Note p' (relativeDuration rd d))
                              <$> lyPitchC p <*> exchDuration d
cElement (Rest d)         = (\rd -> Rest $ relativeDuration rd d)
                              <$> exchDuration d
cElement (Spacer d)       = (\rd -> Spacer $ relativeDuration rd d)
                              <$> exchDuration d
cElement (Chord _ _)      = error "Chord"
cElement (GraceNotes _)   = error "GraceNotes"



-- Pitch is always treated as relative (wrap with a newtype to get absolute)
instance LyPitchC Pitch where
  lyPitchC new = exchPitch new >>= \old -> return $ relPitch old new
  lyPitchesC _xs = error $ "lyPitches -- todo"


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
relativeDuration old drn | old == drn = -1
                         | otherwise = drn


-- LilyPond middle c is c' (aka `c 1`)
-- Mullein middle c is c4
absPitch :: Pitch -> Pitch -> Pitch
absPitch _ (Pitch l a o) = Pitch l a (o-3)


relPitch :: Pitch -> Pitch -> Pitch
relPitch old pch@(Pitch l a _) = Pitch l a (pch `octaveDist` old)
