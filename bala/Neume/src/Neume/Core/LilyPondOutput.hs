{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleInstances          #-}
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
    LyOptionalDuration(..)

  , LyStdGlyph
  , LyStdNote

  , lyRelativeRewrite
  , lyAbsoluteRewrite

  , renderPhrase
  , renderGlyph


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
import Neume.Core.SyntaxInterim
import Neume.Core.SyntaxGlyph
import Neume.Core.Utils
import Neume.Core.Utils.OneList


import Text.PrettyPrint.Leijen          -- package: wl-print

import qualified Data.Foldable          as F




-- Type changing operation ...
--
class LyOptionalDuration gly where
  type LyOptDuration gly :: *
  lyExtractDuration :: gly -> Duration
  toOptDuration     :: Maybe Duration -> gly -> LyOptDuration gly



-- Note graces are always written with their duration...
--
instance LyOptionalDuration (Glyph anno pch Duration) where
  type LyOptDuration (Glyph anno pch Duration) = Glyph anno pch (Maybe Duration)
  
  lyExtractDuration (GlyNote  (Note _ _ d) _) = d
  lyExtractDuration (Rest     d)              = d
  lyExtractDuration (Spacer   d)              = d
  lyExtractDuration (Chord _ d _)             = d
  lyExtractDuration (Graces _)                = dZero

  toOptDuration od (GlyNote  (Note a p _) t) = GlyNote (Note a p od) t 
  toOptDuration od (Rest     _)              = Rest od
  toOptDuration od (Spacer   _)              = Spacer od
  toOptDuration od (Chord xs _ t)            = Chord xs od t
  toOptDuration _  (Graces xs)               = Graces (fmap (fmap3c Just) xs)


instance LyOptionalDuration (MarkupGlyph gly Duration) where
  type LyOptDuration (MarkupGlyph gly Duration) = MarkupGlyph gly (Maybe Duration)

  lyExtractDuration (MGlyph _ d) = d
  lyExtractDuration (Skip d)     = d

  toOptDuration od (MGlyph gly _) = MGlyph gly od
  toOptDuration od (Skip _)       = Skip od



type LyStdGlyph anno = Glyph anno Pitch (Maybe Duration)
type LyStdNote  anno = Note  anno Pitch (Maybe Duration)



-- This isn\'t right - relative pitch transform needs to return
-- the final pitch so the trasformation can be \'stacked\' for 
-- successive phrases.

-- Chaining doesn't work well if we use 
-- Ly_Relative_Rewrite_Config...

lyRelativeRewrite :: Pitch
                  -> CPhrase (Glyph anno Pitch Duration)
                  -> (CPhrase (Glyph anno Pitch (Maybe Duration)), Pitch)
lyRelativeRewrite pch = fmap2a rewriteDurationOpt . rewritePitchRel pch


lyAbsoluteRewrite :: Int 
                  -> CPhrase (Glyph anno Pitch Duration)
                  -> CPhrase (Glyph anno Pitch (Maybe Duration))
lyAbsoluteRewrite i = rewriteDurationOpt . rewritePitchAbs i


--------------------------------------------------------------------------------
-- Render

-- Note for lilypond percussion we might want either the long or 
-- short name printing, so renderPhrase isn't a good candidate 
-- for a Type Class.

-- ignore annotations at the moment...

-- renderPhrase show be parameterized with a function :: (gly -> Doc)...


renderPhrase :: (gly -> Doc) -> CPhrase gly -> PhraseImage
renderPhrase = oPhrase

oPhrase :: (gly -> Doc) -> CPhrase gly -> PhraseImage
oPhrase f (Phrase name bars) = 
    PhraseImage name $ map (oBar f) bars

oBar :: (gly -> Doc) -> CBar gly -> BarImage
oBar f = hsep . oCExprList f 


oCExprList ::  (gly -> Doc) -> [CExpr gly] -> [Doc]
oCExprList f  = map (oCExpr f) 

oCExpr :: (gly -> Doc) -> CExpr gly -> Doc
oCExpr f (Atom e)         = f e 
oCExpr f (N_Plet mp xs)   = pletForm mp (oCExprList f xs)
oCExpr f (Beamed notes)   = beamForm $ oCExprList f notes

-- annos gerally printed _after_ duration...

renderGlyph :: (pch -> Doc) -> Glyph anno pch (Maybe Duration) -> Doc
renderGlyph = oGlyph

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


{-

oSkipGlyph :: (gly -> Maybe Duration -> Doc) 
           -> SkipGlyph gly (Maybe Duration) 
           -> Doc
oSkipGlyph f (SGlyph g d) = f g d
oSkipGlyph _ (Skip d)     = spacer d 

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

default_duration :: Duration
default_duration = qn


-- This one is more complicated than expected...
-- It has to look at the first 'note' of a bar - the first note
-- may be the first note inside a Tuplet or beam group
-- so it is not a standard map (nor a firstSpecial_st either).
--
-- Suggests bringing back shape/contents traversals... ?
-- 

rewriteDurationOpt :: (LyOptionalDuration gly, gly' ~ LyOptDuration gly)
                   => CPhrase gly -> CPhrase gly'
rewriteDurationOpt = 
    stmapBarInitialGlyph doptGlyph1 doptGlyphN default_duration



-- Never replace the duration of the first note in a bar.
--
doptGlyph1 :: (LyOptionalDuration gly, gly' ~ LyOptDuration gly)
           => Duration -> gly -> (gly',Duration)
doptGlyph1 _ gly = (gly',d) 
  where
    d    = lyExtractDuration gly
    gly' = toOptDuration (Just d) gly

-- Never replace the duration of the first note in a bar.
--
doptGlyphN :: (LyOptionalDuration gly, gly' ~ LyOptDuration gly)
           => Duration -> gly -> (gly',Duration)
doptGlyphN old gly = (gly',d) 
  where
    d    = lyExtractDuration gly
    gly' = if (d==old && notDotted d) 
             then toOptDuration Nothing  gly
             else toOptDuration (Just d) gly



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
                -> CPhrase (Glyph anno Pitch dur)
                -> CPhrase (Glyph anno Pitch dur)
rewritePitchAbs i = mapCPhrase (abspGlyph i)


rewritePitchAbs_treble :: CPhrase (Glyph anno Pitch dur)
                       -> CPhrase (Glyph anno Pitch dur)
rewritePitchAbs_treble = rewritePitchAbs (-3)

rewritePitchAbs_tab :: CPhrase (Glyph anno Pitch dur)
                    -> CPhrase (Glyph anno Pitch dur)
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

rewritePitchRel :: Pitch 
                -> CPhrase (Glyph anno Pitch dur)
                -> (CPhrase (Glyph anno Pitch dur), Pitch)
rewritePitchRel pch = stmapCPhrase relpGlyph pch


relpGlyph :: Pitch -> Glyph anno Pitch dur -> (Glyph anno Pitch dur,Pitch)
relpGlyph = stmap3b relpP


-- | Need to return the \original\ pitch as the state, not the
-- octave modified new value.
--
relpP :: Pitch -> Pitch -> (Pitch,Pitch)
relpP prev p = let p' = setOctave (lyOctaveDist prev p) p in (p',p)

