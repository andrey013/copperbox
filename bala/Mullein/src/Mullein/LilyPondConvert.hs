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


import Mullein.Duration
import qualified Mullein.LilyPondSyntax as L
import Mullein.Pitch hiding ( PitchLetter(..) )
import Mullein.RS
import Mullein.ScoreSyntax

import Control.Applicative
import Data.Ratio

data S = S { relative_pitch :: Pitch, relative_duration :: Duration }
data E = E { pitchConvert :: Pitch -> Pitch -> Pitch }


type CM a = RS S E a

convertToLy :: (Pitch -> Pitch -> Pitch) -> Pitch -> Part Element -> Part L.Element
convertToLy f rp e = evalRS (cPart e) s0 e0 where
    s0 = S rp (1%4)
    e0 = E f 

cPart :: Part Element -> CM (Part L.Element)
cPart (Part as)           = Part <$> mapM cPhrase as


cPhrase :: Phrase Element -> CM (Phrase L.Element)
cPhrase (Phrase a)        = Phrase <$> cMotif a
cPhrase (Repeated a)      = Repeated <$> cMotif a
cPhrase (FSRepeat a x y)  = FSRepeat <$> cMotif a <*> cMotif x <*> cMotif y


cMotif :: Motif Element -> CM (Motif L.Element)
cMotif (Motif k m as)       = Motif k m <$> mapM cBar as


-- Reset the duration at the start of the bar.
-- Although not strictly necessary, this makes deducing the duration
-- of a note in a the midst of generated score easier.
 
cBar :: Bar Element -> CM (Bar L.Element)
cBar (Bar a)              = Bar <$> (resetDuration *> cUnison a)
cBar (Overlay a as)       = Overlay <$> (resetDuration *> cUnison a) 
                                    <*> mapM cUnison as

cUnison :: Unison Element -> CM (Unison L.Element)
cUnison (Unison as tied)  = (\xs -> Unison xs tied) 
                              <$> mapM cBracket as

cBracket :: Bracket Element -> CM (Bracket L.Element)
cBracket (Singleton a)    = Singleton <$> cElement a
cBracket (Bracket as)     = Bracket   <$> mapM cElement as


cElement :: Element -> CM L.Element
cElement (Note p d)       = (\f rp rd -> 
                             L.Note (f rp p) (relativeDuration rd d))
                              <$> asks pitchConvert <*> exchPitch p 
                                                    <*> exchDuration d
cElement (Rest d)         = (\rd -> L.Rest $ relativeDuration rd d)
                              <$> exchDuration d
cElement (Spacer d)       = (\rd -> L.Spacer $ relativeDuration rd d)
                              <$> exchDuration d
cElement (Chord _ _)      = error "L.Chord"
cElement (GraceNotes _)   = error "L.GraceNotes"





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


relativeDuration :: Duration -> Duration -> Maybe Duration
relativeDuration ud drn | ud == drn = Nothing
                        | otherwise = Just drn


-- LilyPond middle c is c' (aka `c 1`)
-- Mullein middle c is c4
absPitch :: Pitch -> Pitch -> Pitch
absPitch _ (Pitch l a o) = Pitch l a (o-3)


relPitch :: Pitch -> Pitch -> Pitch
relPitch rel pch@(Pitch l a _) = Pitch l a (pch `octaveDist` rel)
