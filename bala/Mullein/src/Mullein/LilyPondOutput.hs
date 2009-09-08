{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Mullein.LilyPondOutput
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Pretty print LilyPond
--
--------------------------------------------------------------------------------


module Mullein.LilyPondOutput 
  (
  -- * Glyph class
    LilyPondGlyph(..)

  -- * Render    
  , renderPhrase
  , oLyGlyph

  -- * Rewriting
  , ChangeDurationLR(..)
  , rewriteDuration
  
  , ChangePitchLR(..)
  , rewritePitch

  -- * Post-process and pretty print
  , simpleOutput 

   
  ) where

import Mullein.Core
import Mullein.Duration
import Mullein.LilyPondDoc
import Mullein.Pitch
import Mullein.Utils

import MonadLib.Monads

import Text.PrettyPrint.Leijen

import Control.Monad
import qualified Data.Traversable as T




--------------------------------------------------------------------------------
-- Classes

-- | To be renderable as LilyPond, glyphs must implement this class.
class LilyPondGlyph e where
  lyGlyph :: e -> Doc

instance LilyPondGlyph (Glyph Pitch (Maybe Duration)) where
  lyGlyph = oLyGlyph pitch



--------------------------------------------------------------------------------
-- Render

-- | Render a phrase. This function returns a 'DPhrase' which is 
-- a list of list of Doc. To generate output, it must be 
-- post-processed. One such post-processor is 'simpleOutput'...
renderPhrase :: LilyPondGlyph e => Phrase e -> DPhrase
renderPhrase = map oBarOverlay


oBarOverlay :: LilyPondGlyph e => Bar e -> DBar
oBarOverlay (Bar xs)       = [fillSep $ map omBeam xs]
oBarOverlay (OverlayL xss) = map (fillSep . map omBeam) xss


omBeam :: LilyPondGlyph e => Pulse e -> Doc
omBeam (Pulse e)    = lyGlyph e
omBeam (BeamedL es) = beamForm $ map lyGlyph es



oLyGlyph :: (pch -> Doc) -> Glyph pch (Maybe Duration) -> Doc
oLyGlyph f (Note p d t)     = f p <> maybe empty duration d <> optDoc t tie
oLyGlyph _ (Rest d)         = rest d
oLyGlyph _ (Spacer d)       = spacer d
oLyGlyph f (Chord ps d t)   = chordForm (map f ps) d <> optDoc t tie
oLyGlyph f (GraceNotes xs)  = graceForm (map (oGrace f) xs)

oGrace :: (pch -> Doc) -> GraceNote pch (Maybe Duration) -> Doc
oGrace f (GraceNote p d) = f p <> maybe empty duration d



--------------------------------------------------------------------------------
-- rewriting

-- Duration


-- The duration change for LilyPond is stateful (it is 
-- relative to the previous duration), so we have to pass the 
-- state around c.f. unfoldr or the State monad. 
-- Having a simple ChangeDuration class is not useful here. Such 
-- a class might be something like:
--
-- @ class ChangeDuration t where
-- @   changeDuration :: d -> t Duration -> t d
--

-- mnemonic LR - (L)ilypond (R)elative

class ChangeDurationLR t where
  changeDurationLR :: Duration -> t Duration -> (t (Maybe Duration), Duration)

 
rewriteDuration :: ChangeDurationLR t
                => Phrase (t Duration) -> Phrase (t (Maybe Duration))
rewriteDuration bars = fst $ runState dZero (mapM (T.mapM fn) bars)
  where
    fn gly = inside $ (changeDurationLR `flip`  gly)    


instance ChangeDurationLR (Glyph pch) where
  changeDurationLR d0 (Note p d t)     = (Note p (alterDuration d0 d) t, d)
  changeDurationLR d0 (Rest d)         = (Rest (alterDuration d0 d), d)
  changeDurationLR d0 (Spacer d)       = (Spacer (alterDuration d0 d), d)
  changeDurationLR d0 (Chord ps d t)   = (Chord ps (alterDuration d0 d) t, d)
  changeDurationLR d0 (GraceNotes xs)  = (GraceNotes xs',d')
                                          where (xs',d') = changeGraceD d0 xs



changeGraceD :: Duration 
             -> [GraceNote note Duration] 
             -> ([GraceNote note (Maybe Duration)],Duration)
changeGraceD d0 xs = anaMap fn d0 xs where
  fn (GraceNote p drn) d = Just (GraceNote p (alterDuration d drn),d)


alterDuration :: Duration -> Duration -> Maybe Duration
alterDuration d0 d | d0 == d && not (isDotted d) = Nothing
                   | otherwise                   = Just d 

    

-- Relative Pitch

class ChangePitchLR t where
  changePitchLR :: HasPitch pch => Pitch -> t pch drn -> (t pch drn, Pitch)

 
rewritePitch :: (ChangePitchLR t, HasPitch pch) 
             => Pitch -> Phrase (t pch drn) -> Phrase (t pch drn)
rewritePitch rp bars = fst $ runState rp (mapM (T.mapM fn) bars)
  where
    fn gly = inside (changePitchLR `flip` gly)    

inside :: (s -> (a,s)) -> State s a
inside f = get >>= \s -> let (a,s') = f s in set s' >> return a

instance ChangePitchLR Glyph where
  changePitchLR p0 (Note p d t)     = (Note p' d t, getPitch p)
                                      where p' = alterPitch p0 p
  changePitchLR p0 (Rest d)         = (Rest d, p0)
  changePitchLR p0 (Spacer d)       = (Spacer d, p0)
  changePitchLR p0 (Chord ps d t)   = (Chord ps' d t,p') 
                                       where (ps',p') = changeChordP p0 ps
  changePitchLR p0 (GraceNotes xs)  = (GraceNotes xs',p')
                                       where (xs',p') = changeGraceP p0 xs



-- /Relative Pitch/ - all notes inside a grace expression change 
-- relative pitch. Only the first note of a chord changes relative 
-- pitch.


changeGraceP :: HasPitch pch 
             => Pitch -> [GraceNote pch d] -> ([GraceNote pch d],Pitch)
changeGraceP p0 xs = anaMap fn p0 xs where
  fn (GraceNote pch d) p = Just (GraceNote (alterPitch p pch) d, getPitch pch)

-- return changed pitches, plus first pitch of the /original/ chord
changeChordP :: HasPitch pch => Pitch -> [pch] -> ([pch],Pitch)
changeChordP p0 []       = ([],p0)
changeChordP p0 xs@(p:_) = (fst $ anaMap fn p0 xs, getPitch p) where
  fn pch st = Just (alterPitch st pch, getPitch pch)


alterPitch :: HasPitch pch => Pitch -> pch -> pch
alterPitch p0 pch = setPitch p1 pch
  where
    p  = getPitch pch
    p1 = changeOctave p0 p 
    

changeOctave :: Pitch -> Pitch -> Pitch
changeOctave p p' = modifyOctave (lyOctaveDist p p') p'
 



--------------------------------------------------------------------------------
-- helpers



simpleOutput :: DPhrase -> Doc
simpleOutput = vsep . map ((<+> singleBar) . simpleOverlay)


simpleOverlay :: [Doc] -> Doc
simpleOverlay []  = empty
simpleOverlay [a] = a
simpleOverlay xs  = overlay xs




