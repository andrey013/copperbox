{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  M2.LilyPondOutput
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


module M2.LilyPondOutput where 

import M2.Doc
import M2.Duration
import M2.LilyPondDoc
import M2.OneList
import M2.Pitch
import M2.Syntax


import Text.PrettyPrint.Leijen          -- package: wl-print

import qualified Data.Foldable          as F




--------------------------------------------------------------------------------
-- Render

-- ignore annotations at the moment...

oStaffPhrase :: (pch -> Doc) 
             -> StaffPhrase anno pch (Maybe Duration) -> LyPhrase
oStaffPhrase f            = Phrase . map (oStaffBar f) . getPhrase

oStaffBar :: (pch -> Doc) -> StaffBar anno pch (Maybe Duration) -> LyBar
oStaffBar f               = Bar . oBarUnit f . getBar

oBarUnit :: (pch -> Doc) -> BarUnit anno pch (Maybe Duration) -> Doc
oBarUnit f os             = hsep $ toListF (oCExpr f) os

oCExpr :: (pch -> Doc) -> CExpr anno pch (Maybe Duration) -> Doc
oCExpr f (Atomic os)      = hsep $ toListF (oAExpr f) os 
oCExpr _ (N_Plet _ _)     = error $ "oCExpr - N_Plet to do"
oCExpr f (Beamed e)       = beamForm [oCExpr f e]


oAExpr :: (pch -> Doc) -> AExpr anno pch (Maybe Duration) -> Doc
oAExpr f (Glyph e)        = oGlyph f e
oAExpr f (Grace os)       = graceForm $ toListF (oNote f) os


oGlyph :: (pch -> Doc) -> Glyph anno pch (Maybe Duration) -> Doc
oGlyph f (GlyNote n t)    = oNote f n <> optDoc t tie
oGlyph _ (Rest d)         = rest d
oGlyph _ (Spacer d)       = spacer d
oGlyph f (Chord ps d t)   = chordForm (oChordPitches f ps) d <> optDoc t tie


oNote :: (pch -> Doc) -> Note anno pch (Maybe Duration) -> Doc
oNote f (Note _ p d)      =  f p <> maybe empty duration d

oChordPitches :: (pch -> Doc) -> OneList (ChordPitch anno pch) -> [Doc]
oChordPitches f = map (\(ChordPitch _ p) -> f p) . F.toList



--------------------------------------------------------------------------------
-- Doc helpers



simpleOutput :: PhraseDoc -> Doc
simpleOutput = vsep . map ((<+> singleBar) . simpleOverlay) . getPhraseDoc


simpleOverlay :: BarDoc -> Doc
simpleOverlay = step . getBarDoc where
    step []  = empty
    step [a] = getOverlayDoc a
    step xs  = overlay $ map getOverlayDoc xs




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
-- (explanation needed! [Currently, I've forgetten why...])
--


doptBarUnit :: BarUnit anno pch Duration
           -> Duration
           -> (BarUnit anno pch (Maybe Duration), Duration)
doptBarUnit = accumMapL doptCExpr

doptCExpr  :: CExpr anno pch Duration
           -> Duration
           -> (CExpr anno pch (Maybe Duration), Duration)
doptCExpr ce d0 = step ce where
  step (Atomic os)      = let (os',st)  = accumMapL doptAExpr os d0
                          in (Atomic os', st)
  step (N_Plet np expr) = let (expr',st) = doptCExpr expr d0 in (N_Plet np expr', st)
  step (Beamed expr)    = let (expr',st) = doptCExpr expr d0 in (Beamed expr', st)
  

doptAExpr :: AExpr anno pch Duration
          -> Duration
          -> (AExpr anno pch (Maybe Duration), Duration)
doptAExpr ae d0 = step ae where
  step (Glyph e)        = let (e',st) = doptGlyph e d0 in (Glyph e', st)
  step (Grace os)       = let (os',st) = accumMapL doptNote os d0
                          in (Grace os',st)


doptGlyph :: Glyph anno pch Duration 
          -> Duration 
          -> (Glyph anno pch (Maybe Duration), Duration)
doptGlyph g d0 = step g where
  step (GlyNote n t)  = let (n',st) = doptNote n d0 in (GlyNote n' t, st)
  step (Rest d)       = let (d',st) = doptD d d0 in (Rest d',st)
  step (Spacer d)     = let (d',st) = doptD d d0 in (Spacer d',st)
  step (Chord os d t) = let (d',st) = doptD d d0 in (Chord os d' t, st)


doptNote :: Note anno pch Duration 
         -> Duration 
         -> (Note anno pch (Maybe Duration), Duration)
doptNote (Note a p d) d0 = let (d',st) = doptD d d0 in (Note a p d', st)


doptD :: Duration -> Duration -> (Maybe Duration, Duration)
doptD d st | d == st && not (isDotted d) = (Nothing,st)
           | otherwise                   = (Just d,d) 


doptSimpleBarUnit :: SimpleBarUnit glyph Duration
                  -> Duration
                  -> (SimpleBarUnit glyph (Maybe Duration), Duration)
doptSimpleBarUnit = accumMapL doptSkipGlyph


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


abspBarUnit :: Int -> BarUnit anno Pitch dur -> BarUnit anno Pitch dur
abspBarUnit i = fmap (abspCExpr i)


abspCExpr  :: Int -> CExpr anno Pitch dur -> CExpr anno Pitch dur
abspCExpr i (Atomic os)      = Atomic $ fmap (abspAExpr i) os
abspCExpr i (N_Plet np expr) = N_Plet np $ abspCExpr i expr
abspCExpr i (Beamed expr)    = Beamed $ abspCExpr i expr


abspAExpr :: Int -> AExpr anno Pitch dur -> AExpr anno Pitch dur
abspAExpr i (Glyph e)      = Glyph $ abspGlyph i e
abspAExpr i (Grace os)     = Grace $ fmap (abspNote i) os


abspGlyph :: Int -> Glyph anno Pitch dur -> Glyph anno Pitch dur
abspGlyph i (GlyNote n t)  = GlyNote (abspNote i n) t
abspGlyph _ (Rest d)       = Rest d
abspGlyph _ (Spacer d)     = Spacer d
abspGlyph i (Chord os d t) = Chord (fmap (abspChordPitch i) os) d t


abspNote :: Int -> Note anno Pitch dur -> Note anno Pitch dur
abspNote i (Note a p d) = Note a (displaceOctave i p) d

abspChordPitch :: Int -> ChordPitch anno Pitch -> ChordPitch anno Pitch
abspChordPitch i (ChordPitch a p) = ChordPitch a (displaceOctave i p)

--------------------------------------------------------------------------------
-- Relative Pitch



relpBarUnit :: BarUnit anno Pitch dur 
            -> Pitch 
            -> (BarUnit anno Pitch dur, Pitch)
relpBarUnit = accumMapL relpCExpr 


relpCExpr  :: CExpr anno Pitch dur 
           -> Pitch
           -> (CExpr anno Pitch dur, Pitch)
relpCExpr (Atomic os)   p0 = let (os',p) = accumMapL relpAExpr os p0 in
                             (Atomic os', p)
relpCExpr (N_Plet np e) p0 = let (e',p) = relpCExpr e p0 in (N_Plet np e', p)
relpCExpr (Beamed e)    p0 = let (e',p) = relpCExpr e p0 in (Beamed e', p)


relpAExpr :: AExpr anno Pitch dur 
          -> Pitch
          -> (AExpr anno Pitch dur, Pitch)
relpAExpr (Glyph e)  p0    = let (e',p) = relpGlyph e p0 in (Glyph e', p)
relpAExpr (Grace os) p0    = let (os',p) = accumMapL relpNote os p0 in
                             (Grace os', p)

relpGlyph :: Glyph anno Pitch dur 
          -> Pitch
          -> (Glyph anno Pitch dur, Pitch)
relpGlyph g p0 = step g where
  step (GlyNote n t)  = let (n',p) = relpNote n p0 in (GlyNote n' t,p)
  step (Rest d)       = (Rest d, p0)
  step (Spacer d)     = (Spacer d, p0)
  step (Chord os d t) = let (os', p) = accumMapL relpChordPitch os p0 
                        in (Chord os' d t, p)


relpNote :: Note anno Pitch dur -> Pitch -> (Note anno Pitch dur,Pitch)
relpNote (Note a p d) p0 = let p' = relpP p0 p in (Note a p' d, p')

relpChordPitch :: ChordPitch anno Pitch -> Pitch -> (ChordPitch anno Pitch, Pitch)
relpChordPitch (ChordPitch a p) p0 = let p' = relpP p0 p in (ChordPitch a p', p')


relpP :: Pitch -> Pitch -> Pitch
relpP p0 p = setOctave (lyOctaveDist p0 p) p

