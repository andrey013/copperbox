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

  -- * Render    
    PDGlyphLy
  , renderPhrase
  , lyGlyph
  , oLyGlyph

  -- * Rewriting
  , ChangeDurationLyRel(..)
  , rewriteDuration
  , alterDuration  

  , ChangePitchLyRel(..)
  , rewritePitch

  , ChangePitchLyAbs(..)
  , rewritePitchAbs

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
-- Render

type PDGlyphLy = Glyph () Pitch (Maybe Duration)

-- | Render a phrase. This function returns a 'DPhrase' which is 
-- a list of list of Doc. To generate output, it must be 
-- post-processed. One such post-processor is 'simpleOutput'...
renderPhrase :: (e -> Doc) -> Phrase e -> DPhrase
renderPhrase f = map (renderBarOverlay f)


renderBarOverlay :: (e -> Doc) -> Bar e -> DBar
renderBarOverlay f (Bar xs)       = [fillSep $ map (renderBeam f) xs]
renderBarOverlay f (OverlayL xss) = map (fillSep . map (renderBeam f)) xss


renderBeam :: (e -> Doc) -> Pulse e -> Doc
renderBeam f (Pulse e)    = f e
renderBeam f (BeamedL es) = beamForm $ map f es


lyGlyph :: PDGlyphLy -> Doc
lyGlyph = oLyGlyph pitch


oLyGlyph :: (pch -> Doc) -> Glyph anno pch (Maybe Duration) -> Doc
oLyGlyph f (Note _ p d t)   = f p <> maybe empty duration d <> optDoc t tie
oLyGlyph _ (Rest d)         = rest d
oLyGlyph _ (Spacer d)       = spacer d
oLyGlyph f (Chord ps d t)   = chordForm (map (f . snd) ps) d <> optDoc t tie
oLyGlyph f (GraceNotes xs)  = graceForm (map (oGrace f) xs)

oGrace :: (pch -> Doc) -> GraceNote anno pch (Maybe Duration) -> Doc
oGrace f (GraceNote _ p d) = f p <> maybe empty duration d



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

class ChangeDurationLyRel t where
  changeDurationLyRel :: Duration -> t Duration -> (t (Maybe Duration), Duration)

 
rewriteDuration :: ChangeDurationLyRel t
                => Phrase (t Duration) -> Phrase (t (Maybe Duration))
rewriteDuration bars = fst $ runState dZero (mapM (T.mapM fn) bars)
  where
    fn gly = inside $ (changeDurationLyRel `flip`  gly)    


instance ChangeDurationLyRel (Glyph anno pch) where
  changeDurationLyRel d0 (Note anno p d t) = (Note anno p (alterDuration d0 d) t, d)
  changeDurationLyRel d0 (Rest d)          = (Rest (alterDuration d0 d), d)
  changeDurationLyRel d0 (Spacer d)        = (Spacer (alterDuration d0 d), d)
  changeDurationLyRel d0 (Chord ps d t)    = (Chord ps (alterDuration d0 d) t, d)
  changeDurationLyRel d0 (GraceNotes xs)   = (GraceNotes xs',d')
                                             where (xs',d') = changeGraceD d0 xs



changeGraceD :: Duration 
             -> [GraceNote anno note Duration] 
             -> ([GraceNote anno note (Maybe Duration)],Duration)
changeGraceD d0 xs = anaMap fn d0 xs where
  fn (GraceNote a p drn) d = Just (GraceNote a p (alterDuration d drn),d)


alterDuration :: Duration -> Duration -> Maybe Duration
alterDuration d0 d | d0 == d && not (isDotted d) = Nothing
                   | otherwise                   = Just d 

    

-- Relative Pitch

class ChangePitchLyRel t where
  changePitchLyRel :: HasPitch pch => Pitch -> t pch drn -> (t pch drn, Pitch)

 
rewritePitch :: (ChangePitchLyRel t, HasPitch pch) 
             => Pitch -> Phrase (t pch drn) -> Phrase (t pch drn)
rewritePitch rp bars = fst $ runState rp (mapM (T.mapM fn) bars)
  where
    fn gly = inside (changePitchLyRel `flip` gly)    

inside :: (s -> (a,s)) -> State s a
inside f = get >>= \s -> let (a,s') = f s in set s' >> return a

instance ChangePitchLyRel (Glyph anno) where
  changePitchLyRel p0 (Note a p d t)   = (Note a p' d t, getPitch p)
                                         where p' = alterPitch p0 p
  changePitchLyRel p0 (Rest d)         = (Rest d, p0)
  changePitchLyRel p0 (Spacer d)       = (Spacer d, p0)
  changePitchLyRel p0 (Chord ps d t)   = (Chord ps' d t,p') 
                                         where (ps',p') = changeChordP p0 ps
  changePitchLyRel p0 (GraceNotes xs)  = (GraceNotes xs',p')
                                         where (xs',p') = changeGraceP p0 xs



-- /Relative Pitch/ - all notes inside a grace expression change 
-- relative pitch. Only the first note of a chord changes relative 
-- pitch.


changeGraceP :: HasPitch pch 
             => Pitch -> [GraceNote anno pch d] -> ([GraceNote anno pch d],Pitch)
changeGraceP p0 xs = anaMap fn p0 xs where
  fn (GraceNote anno pch d) p = Just (GraceNote anno pch' d, getPitch pch)
                                where pch' = alterPitch p pch

-- return changed pitches, plus first pitch of the /original/ chord
changeChordP :: HasPitch pch => Pitch -> [(anno,pch)] -> ([(anno,pch)],Pitch)
changeChordP p0 []            = ([],p0)
changeChordP p0 xs@((_,p):_)  = (fst $ anaMap fn p0 xs, getPitch p) where
  fn (anno,pch) st = Just ((anno,alterPitch st pch), getPitch pch)


alterPitch :: HasPitch pch => Pitch -> pch -> pch
alterPitch p0 pch = setPitch p1 pch
  where
    p  = getPitch pch
    p1 = changeOctave p0 p 



changeOctave :: Pitch -> Pitch -> Pitch
changeOctave p p' = setOctave (lyOctaveDist p p') p'

--------------------------------------------------------------------------------
-- Absolute pitch transformation
    
-- LilyPond Absolute pitch (4 octaves lower than Mullein)
--
-- Middle C in Mullein is C-octave 5.
-- Middle C in LilyPond is c' - in Mullein terms, after the 
-- absolute pitch transformation, this is C-octave 1, the 
-- octave designator is reduced by 4 to represent the number
-- of apostrophes to print (a negative number represents the 
-- number of commas to print, after taking the @abs@ of the 
-- value).
-- 
-- HOWEVER, printing guitar tablature in absolute mode seems to 
-- take middle c as C (C-octave 0), so 5 has to be subtracted 
-- from the octave designator.
--
-- TODO - find out why this is the case.



class ChangePitchLyAbs t where
  changePitchLyAbs :: HasPitch pch => Int -> t pch drn -> t pch drn

 
rewritePitchAbs :: (HasPitch pch, ChangePitchLyAbs t)
                => Int -> Phrase (t pch drn) -> Phrase (t pch drn)
rewritePitchAbs i = fmap (fmap (changePitchLyAbs i)) 


instance ChangePitchLyAbs (Glyph anno) where
  changePitchLyAbs i (Note a p d t)   = Note a (displaceOctave i p) d t
  changePitchLyAbs _ (Rest d)         = Rest d
  changePitchLyAbs _ (Spacer d)       = Spacer d
  changePitchLyAbs i (Chord ps d t)   = Chord (map fn ps) d t 
                                        where fn (a,p) = (a,displaceOctave i p)
  changePitchLyAbs i (GraceNotes xs)  = GraceNotes (fmap (changePitchLyAbs i) xs)

instance ChangePitchLyAbs (GraceNote anno) where
  changePitchLyAbs i (GraceNote a p d)  = GraceNote a (displaceOctave i p) d

--------------------------------------------------------------------------------
-- helpers



simpleOutput :: DPhrase -> Doc
simpleOutput = vsep . map ((<+> singleBar) . simpleOverlay)


simpleOverlay :: [Doc] -> Doc
simpleOverlay []  = empty
simpleOverlay [a] = a
simpleOverlay xs  = overlay xs




