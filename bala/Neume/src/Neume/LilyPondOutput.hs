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

oStaffPhrase :: (pch -> Doc) 
             -> StaffPhrase (Glyph anno pch (Maybe Duration)) -> LyPhrase
oStaffPhrase f            = LyPhrase . map (oStaffBar f) . getStaffPhrase

oStaffBar :: (pch -> Doc) -> StaffBar (Glyph anno pch (Maybe Duration)) -> LyBar
oStaffBar f               = LyBar . oBarUnit f . getStaffBar

oBarUnit :: (pch -> Doc) -> OneList (CExpr (Glyph anno pch (Maybe Duration))) -> Doc
oBarUnit f os             = hsep $ toListF (oCExpr f) os

oCExpr :: (pch -> Doc) -> CExpr (Glyph anno pch (Maybe Duration)) -> Doc
oCExpr f (Atomic os)      = hsep $ toListF (oGlyph f) os 
oCExpr _ (N_Plet _ _)     = error $ "oCExpr - N_Plet to do"
oCExpr f (Beamed e)       = beamForm [oCExpr f e]

oGlyph :: (pch -> Doc) -> Glyph anno pch (Maybe Duration) -> Doc
oGlyph f (GlyNote n t)    = oNote f n <> optDoc t tie
oGlyph _ (Rest d)         = rest d
oGlyph _ (Spacer d)       = spacer d
oGlyph f (Chord ps d t)   = chordForm (oChordPitches f ps) d <> optDoc t tie
oGlyph f (Grace os)       = graceForm $ toListF (oNote f) os


oNote :: (pch -> Doc) -> Note anno pch (Maybe Duration) -> Doc
oNote f (Note _ p d)      =  f p <> maybe empty duration d

oChordPitches :: (pch -> Doc) -> OneList (ChordPitch anno pch) -> [Doc]
oChordPitches f = map (\(ChordPitch _ p) -> f p) . F.toList



--------------------------------------------------------------------------------
-- Doc helpers


{-
simpleOutput :: PhraseDoc -> Doc
simpleOutput = vsep . map ((<+> singleBar) . simpleOverlay) . getPhraseDoc


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
doptStaffPhrase :: StaffPhrase (Glyph anno pch Duration)
                -> StaffPhrase (Glyph anno pch (Maybe Duration))
doptStaffPhrase (StaffPhrase bars) = 
    StaffPhrase $ map (\x -> fst $ doptBarUnit x default_duration) bars
  where
    default_duration = qn 


doptBarUnit :: StaffBar (Glyph anno pch Duration)
           -> Duration
           -> (StaffBar (Glyph anno pch (Maybe Duration)), Duration)
doptBarUnit = stmap doptGlyph


doptGlyph :: Glyph anno pch Duration 
          -> Duration 
          -> (Glyph anno pch (Maybe Duration), Duration)
doptGlyph g d0 = step g where
  step (GlyNote n t)  = let (n',st) = doptNote n d0 in (GlyNote n' t, st)
  step (Rest d)       = let (d',st) = doptD d d0 in (Rest d',st)
  step (Spacer d)     = let (d',st) = doptD d d0 in (Spacer d',st)
  step (Chord os d t) = let (d',st) = doptD d d0 in (Chord os d' t, st)
  step (Grace os)     = let (os',st) = accumMapL doptNote os d0
                        in (Grace os',st)


doptNote :: Note anno pch Duration 
         -> Duration 
         -> (Note anno pch (Maybe Duration), Duration)
doptNote (Note a p d) d0 = let (d',st) = doptD d d0 in (Note a p d', st)


doptD :: Duration -> Duration -> (Maybe Duration, Duration)
doptD d st | d == st && not (isDotted d) = (Nothing,st)
           | otherwise                   = (Just d,d) 


doptMarkupBar :: MarkupBar (SkipGlyph gly Duration)
              -> Duration
              -> (MarkupBar (SkipGlyph gly (Maybe Duration)), Duration)
doptMarkupBar (MarkupBar os) d0 = (MarkupBar os', d) where 
    (os',d) = accumMapL doptSkipGlyph os d0
             


doptSkipGlyph :: SkipGlyph glyph Duration
              -> Duration
              -> (SkipGlyph glyph (Maybe Duration), Duration)
doptSkipGlyph gly d0 = step gly  where
  step (SGlyph g d) = let (d',st) = doptD d d0 in (SGlyph g d',st)
  step (Skip d)     = let (d',st) = doptD d d0 in (Skip d',st)

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


abspStaffBar :: Int 
             -> StaffBar (Glyph anno Pitch dur) 
             -> StaffBar (Glyph anno Pitch dur)
abspStaffBar i (StaffBar os) = StaffBar $ fmap (abspCExpr i) os


abspCExpr  :: Int 
           -> CExpr (Glyph anno Pitch dur) 
           -> CExpr (Glyph anno Pitch dur)
abspCExpr i (Atomic os)      = Atomic $ fmap (abspGlyph i) os
abspCExpr i (N_Plet np expr) = N_Plet np $ abspCExpr i expr
abspCExpr i (Beamed expr)    = Beamed $ abspCExpr i expr



abspGlyph :: Int -> Glyph anno Pitch dur -> Glyph anno Pitch dur
abspGlyph i (GlyNote n t)  = GlyNote (abspNote i n) t
abspGlyph _ (Rest d)       = Rest d
abspGlyph _ (Spacer d)     = Spacer d
abspGlyph i (Chord os d t) = Chord (fmap (abspChordPitch i) os) d t
abspGlyph i (Grace os)     = Grace $ fmap (abspNote i) os


abspNote :: Int -> Note anno Pitch dur -> Note anno Pitch dur
abspNote i (Note a p d) = Note a (displaceOctave i p) d

abspChordPitch :: Int -> ChordPitch anno Pitch -> ChordPitch anno Pitch
abspChordPitch i (ChordPitch a p) = ChordPitch a (displaceOctave i p)

--------------------------------------------------------------------------------
-- Relative Pitch

relpStaffPhrase :: StaffBar (Glyph anno Pitch dur)
                -> Pitch 
                -> (StaffBar (Glyph anno Pitch dur), Pitch)
relpStaffPhrase = stmap relpGlyph

                               


relpGlyph :: Glyph anno Pitch dur 
          -> Pitch
          -> (Glyph anno Pitch dur, Pitch)
relpGlyph g p0 = step g where
  step (GlyNote n t)  = let (n',p) = relpNote n p0 in (GlyNote n' t,p)
  step (Rest d)       = (Rest d, p0)
  step (Spacer d)     = (Spacer d, p0)
  step (Chord os d t) = let (os', p) = accumMapL relpChordPitch os p0 
                        in (Chord os' d t, p)
  step (Grace os)     = let (os',p) = accumMapL relpNote os p0 
                        in  (Grace os', p)


relpNote :: Note anno Pitch dur -> Pitch -> (Note anno Pitch dur,Pitch)
relpNote (Note a p d) p0 = let p' = relpP p0 p in (Note a p' d, p')

relpChordPitch :: ChordPitch anno Pitch -> Pitch -> (ChordPitch anno Pitch, Pitch)
relpChordPitch (ChordPitch a p) p0 = let p' = relpP p0 p in (ChordPitch a p', p')


relpP :: Pitch -> Pitch -> Pitch
relpP p0 p = setOctave (lyOctaveDist p0 p) p

