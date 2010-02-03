{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleInstances          #-}
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



import MonadLib.Monads

import Text.PrettyPrint.Leijen

import qualified Data.Foldable          as F
import qualified Data.Traversable       as T




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

{-

--------------------------------------------------------------------------------
-- rewriting
    
--------------------------------------------------------------------------------
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
-}

type OctaveLy = Int

class ChangePitchLyAbs t where
  changePitchLyAbs :: HasPitch pch => OctaveLy -> t pch drn -> t pch drn

{- 
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

-}




