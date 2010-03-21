{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Neume.Core.LilyPondOutput
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


module Neume.Core.LilyPondOutput 
  (
    renderPhrase
  , oGlyph

  -- * rewriting
  , rewriteDurationOpt
  , rewritePitchAbs
  , rewritePitchAbs_treble
  , rewritePitchAbs_tab
  , rewritePitchRel

  ) where 

import Neume.Core.Duration
import Neume.Core.LilyPondBasic
import Neume.Core.Pitch
import Neume.Core.SyntaxScore
import Neume.Core.SyntaxMarkup () -- TODO trasform and print markup
import Neume.Core.SyntaxStaff
import Neume.Core.Utils.OneList
import Neume.Core.Utils.Pretty
import Neume.Core.Utils.StateMap


import Text.PrettyPrint.Leijen          -- package: wl-print

import qualified Data.Foldable          as F

--------------------------------------------------------------------------------
-- Render

-- Note for lilypond percussion we might want either the long or 
-- short name printing, so renderPhrase isn't a good candidate 
-- for a Type Class.

-- ignore annotations at the moment...
renderPhrase :: (pch -> Doc) -> StaffPhrase (GlyphRelDur anno pch) -> PhraseImage
renderPhrase f = oStaffPhrase f



oStaffPhrase :: (pch -> Doc) -> StaffPhrase (GlyphRelDur anno pch) -> PhraseImage
oStaffPhrase f            = PhraseImage . map (oStaffBar f) . extractBars

oStaffBar :: (pch -> Doc) -> StaffBar (GlyphRelDur anno pch) -> BarImage
oStaffBar f               = hsep . oCExprList f . extractNotes

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

rewriteDurationOpt :: FreeRewrite (Glyph anno pch Duration)
                                  (Glyph anno pch (Maybe Duration))
rewriteDurationOpt  = 
    StaffPhrase . map (fst . (stmap doptGlyph default_duration)) . extractBars
  where
    default_duration = qn 



doptGlyph :: Duration 
          -> Glyph anno pch Duration 
          -> (Glyph anno pch (Maybe Duration), Duration)
doptGlyph = stmap3c doptD



doptD :: Duration -> Duration -> (Maybe Duration, Duration)
doptD st d | d == st && not (isDotted d) = (Nothing,st)
           | otherwise                   = (Just d,d) 

--


{-

-- TO DO - these will be needed for Markup...

doptMarkupBar :: MarkupBar (SkipGlyph gly Duration)
              -> Duration
              -> (MarkupBar (SkipGlyph gly (Maybe Duration)), Duration)
doptMarkupBar = stmap (stmap2b doptD)


doptSkipGlyph :: SkipGlyph glyph Duration
              -> Duration
              -> (SkipGlyph glyph (Maybe Duration), Duration)
doptSkipGlyph = stmap2b doptD
-}

--------------------------------------------------------------------------------
-- Rewrite Pitch

-- Absolute 

-- Middle C in Neume is C-octave 4.
--
-- Middle C in LilyPond is c' - in Mullein terms, after the 
-- absolute pitch transformation, this is C-octave 1, the 
-- octave designator has 3 subtracted to represent the number
-- of apostrophes to print (a negative number represents the 
-- number of commas to print, after taking the @abs@ of the 
-- value).
-- 
-- HOWEVER, printing guitar tablature in absolute mode seems to 
-- take middle c as C (C-octave 0), so 4 has to be subtracted 
-- from the octave designator.
--
-- TODO - find out why this is the case.




rewritePitchAbs :: Int 
                -> FreeRewrite (Glyph anno Pitch dur) (Glyph anno Pitch dur)
rewritePitchAbs i = fmap (abspGlyph i)


rewritePitchAbs_treble :: FreeRewrite (Glyph anno Pitch dur) 
                                      (Glyph anno Pitch dur)
rewritePitchAbs_treble = rewritePitchAbs (-3)

rewritePitchAbs_tab :: FreeRewrite (Glyph anno Pitch dur) (Glyph anno Pitch dur)
rewritePitchAbs_tab = rewritePitchAbs (-4)




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

rewritePitchRel :: CtxRewrite Pitch (Glyph anno Pitch dur) (Glyph anno Pitch dur)
rewritePitchRel pch phrase = stmap relpGlyph pch phrase


relpGlyph :: Pitch -> Glyph anno Pitch dur -> (Glyph anno Pitch dur,Pitch)
relpGlyph = stmap3b relpP


-- | Need to return the \original\ pitch as the state, not the
-- octave modified new value.
--
relpP :: Pitch -> Pitch -> (Pitch,Pitch)
relpP prev p = let p' = setOctave (lyOctaveDist prev p) p in (p,p')

