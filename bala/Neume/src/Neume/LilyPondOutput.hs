{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Neume.LilyPondOutput
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Pretty print LilyPond
--
--------------------------------------------------------------------------------


module Neume.LilyPondOutput where 

import Neume.Doc
import Neume.Duration
import Neume.LilyPondDoc
import Neume.OneList
import Neume.Pitch
import Neume.StateMap
import Neume.SyntaxDoc
import Neume.SyntaxMarkup
import Neume.SyntaxStaff


import Text.PrettyPrint.Leijen          -- package: wl-print

import qualified Data.Foldable          as F

--------------------------------------------------------------------------------
-- Render

-- ignore annotations at the moment...
renderPhrase :: StaffPhrase (GlyphRelDur anno Pitch) -> LyPhrase
renderPhrase = oStaffPhrase pitch


oStaffPhrase :: (pch -> Doc) -> StaffPhrase (GlyphRelDur anno pch) -> LyPhrase
oStaffPhrase f            = LyPhrase . map (oStaffBar f) . getStaffPhrase

oStaffBar :: (pch -> Doc) -> StaffBar (GlyphRelDur anno pch) -> LyBar
oStaffBar f               = LyBar . hsep . oCExprList f . getStaffBar

-- oBarUnit :: (pch -> Doc) -> OneList (CExpr (GlyphRelDur anno pch)) -> Doc
-- oBarUnit f os             = hsep $ toListF (oCExpr f) os

oCExprList ::  (pch -> Doc) -> CExprList (GlyphRelDur anno pch) -> [Doc]
oCExprList f (CExprList xs) = map (oCExpr f) xs

oCExpr :: (pch -> Doc) -> CExpr (GlyphRelDur anno pch) -> Doc
oCExpr f (Atom e)         = oGlyph f e 
oCExpr _ (N_Plet _ _)     = error $ "oCExpr - N_Plet to do"
oCExpr f (Beamed notes)   = beamForm $ oCExprList f notes

oGlyph :: (pch -> Doc) -> GlyphRelDur anno pch -> Doc
oGlyph f (GlyNote n t)    = oNote f n <> optDoc t tie
oGlyph _ (Rest d)         = rest d
oGlyph _ (Spacer d)       = spacer d
oGlyph f (Chord ps d t)   = chordForm (oChordPitches f ps) d <> optDoc t tie
oGlyph f (Graces os)      = graceForm $ toListF (oNote f) os


oNote :: (pch -> Doc) -> NoteRelDur anno pch -> Doc
oNote f (Note _ p d)      =  f p <> maybe empty duration d

oChordPitches :: (pch -> Doc) -> OneList (ChordPitch anno pch) -> [Doc]
oChordPitches f = map (\(ChordPitch _ p) -> f p) . F.toList



--------------------------------------------------------------------------------
-- Doc helpers



simpleOutput :: LyPhrase -> Doc
simpleOutput = vsep . map ((<+> singleBar) . getLyBar) . getLyPhrase

{-
simpleOverlay :: BarDoc -> Doc
simpleOverlay = step . getBarDoc where
    step []  = empty
    step [a] = getOverlayDoc a
    step xs  = overlay $ map getOverlayDoc xs
-}



--------------------------------------------------------------------------------
-- Rewrite Duration

-- LilyPond has a shorthand notation thats a variation on 
-- run-length-encoding - successive equal durations are elided.
--
-- For (post-)composabilty the first note in a bar should be  
-- printed with a duration regardless of the duration of its
-- predecessor.
--
-- Also it doesn't seem useful to 'compact' dotted durations.
-- (explanation needed! [Currently, I've forgotten why...])
--

-- Note - seed each bar with the default duration.
-- This makes scores clearer.
--
rewriteDurationOpt :: StaffPhrase (Glyph anno pch Duration)
                   -> StaffPhrase (Glyph anno pch (Maybe Duration))
rewriteDurationOpt (StaffPhrase bars) = 
    StaffPhrase $ map (fst . (stmap doptGlyph `flip` default_duration)) bars
  where
    default_duration = qn 


doptBarUnit :: StaffBar (Glyph anno pch Duration)
           -> Duration
           -> (StaffBar (Glyph anno pch (Maybe Duration)), Duration)
doptBarUnit = stmap doptGlyph


doptGlyph :: Glyph anno pch Duration 
          -> Duration 
          -> (Glyph anno pch (Maybe Duration), Duration)
doptGlyph = stmap3c doptD



doptD :: Duration -> Duration -> (Maybe Duration, Duration)
doptD d st | d == st && not (isDotted d) = (Nothing,st)
           | otherwise                   = (Just d,d) 

--
doptMarkupBar :: MarkupBar (SkipGlyph gly Duration)
              -> Duration
              -> (MarkupBar (SkipGlyph gly (Maybe Duration)), Duration)
doptMarkupBar = stmap (stmap2b doptD)


doptSkipGlyph :: SkipGlyph glyph Duration
              -> Duration
              -> (SkipGlyph glyph (Maybe Duration), Duration)
doptSkipGlyph = stmap2b doptD


--------------------------------------------------------------------------------
-- Rewrite Pitch

-- Absolute 

-- LilyPond Absolute pitch (4 octaves lower than Mullein)
-- TODO is Mullein same as Haskore??
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

rewritePitchAbs :: Int 
                -> StaffPhrase (Glyph anno Pitch dur) 
                -> StaffPhrase (Glyph anno Pitch dur)
rewritePitchAbs i = fmap (abspGlyph i)



abspGlyph :: Int -> Glyph anno Pitch dur -> Glyph anno Pitch dur
abspGlyph i (GlyNote n t)  = GlyNote (abspNote i n) t
abspGlyph _ (Rest d)       = Rest d
abspGlyph _ (Spacer d)     = Spacer d
abspGlyph i (Chord os d t) = Chord (fmap (abspChordPitch i) os) d t
abspGlyph i (Graces os)    = Graces $ fmap (abspNote i) os


abspNote :: Int -> Note anno Pitch dur -> Note anno Pitch dur
abspNote i (Note a p d) = Note a (displaceOctave i p) d

abspChordPitch :: Int -> ChordPitch anno Pitch -> ChordPitch anno Pitch
abspChordPitch i (ChordPitch a p) = ChordPitch a (displaceOctave i p)

--------------------------------------------------------------------------------
-- Relative Pitch

rewritePitchRel :: Pitch
                -> StaffPhrase (Glyph anno Pitch dur) 
                -> StaffPhrase (Glyph anno Pitch dur)
rewritePitchRel p phrase = fst $ stmap relpGlyph phrase p 


relpStaffPhrase :: StaffPhrase (Glyph anno Pitch dur)
                -> Pitch 
                -> (StaffPhrase (Glyph anno Pitch dur), Pitch)
relpStaffPhrase = stmap relpGlyph

                               


relpGlyph :: Glyph anno Pitch dur 
          -> Pitch
          -> (Glyph anno Pitch dur, Pitch)
relpGlyph = stmap3b relpP

relpP :: Pitch -> Pitch -> (Pitch,Pitch)
relpP p0 p = (setOctave (lyOctaveDist p0 p) p,p)

