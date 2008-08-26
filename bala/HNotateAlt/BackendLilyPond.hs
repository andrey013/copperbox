{-# LANGUAGE FlexibleContexts #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  BackendLilyPond
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Emit LilyPond from Score representation.
--
--------------------------------------------------------------------------------

module BackendLilyPond (
    Notate_Ly_Env(..), 
    default_ly_env,
    translateLilyPond
  ) where


import CommonUtils
import Duration hiding (breve,longa)
import TextLilyPond hiding (duration)
import qualified TextLilyPond as Ly
import NotateMonad
import Pitch
import ScoreRepresentation
import Traversals

import Control.Applicative
import Control.Monad.Reader hiding (mapM)
import Control.Monad.State hiding (mapM)
import qualified Data.Foldable as F
import Data.Monoid
import Data.Sequence hiding (reverse)
import Data.Traversable
import Prelude hiding (mapM)
import qualified Text.PrettyPrint.Leijen as PP

newtype LilyPondExprs = LilyPondExprs { 
    getLilyPondExprs :: Seq LilyPondMusicLine
  }
  
type LilyPondMusicLine = LyCxt_Element

instance PP.Pretty LilyPondExprs where
  pretty (LilyPondExprs se) = F.foldl fn PP.empty se
    where fn a e = a PP.<$> PP.text "____" PP.<$> getLy e



type ProcessM a = NotateM Notate_Ly_State Notate_Ly_Env a


data LilyPondPitch = LilyPondPitch {
    ly_pitchletter  :: LyPitchName,
    ly_accidental   :: Maybe LyAccidental,
    ly_octave       :: Maybe LyOctaveSpec
  }
  
data LilyPondDuration = LilyPondDuration {
    ly_duration       :: Maybe LyDuration
  }
  
  
type LilyPondGlyph = Glyph LilyPondPitch LilyPondDuration
                
                 
type LySystem    = ScSystem   LilyPondGlyph
type LyStrata    = ScStrata   LilyPondGlyph
type LyBlock     = ScBlock    LilyPondGlyph
type LyMeasure   = ScMeasure  LilyPondGlyph




data Notate_Ly_State = Notate_Ly_State {
    relative_pitch      :: Pitch,
    relative_duration   :: Duration
  }


data Notate_Ly_Env = Notate_Ly_Env {
    initial_ly_context        :: LyCxt_Element,
    initial_relative_pitch    :: Pitch
  }


state0 :: Notate_Ly_State
state0 = Notate_Ly_State {
    relative_pitch      = middleC,
    relative_duration   = semiquaver
  }

default_ly_env :: Notate_Ly_Env
default_ly_env = Notate_Ly_Env {
    initial_ly_context        = elementStart,
    initial_relative_pitch    = middleC
  }





olyDuration :: Duration -> Maybe LyDuration
olyDuration d = let (nr,dr,dots) = durationElements d in fn nr dr
  where 
    fn 4 1      = Just longa  
    fn 2 1      = Just breve 
    fn 1 1      = Just $ Ly.duration 1
    fn 1 2      = Just $ Ly.duration 2
    fn 1 4      = Just $ Ly.duration 4
    fn 1 8      = Just $ Ly.duration 8
    fn 1 16     = Just $ Ly.duration 16
    fn 1 32     = Just $ Ly.duration 32
    fn 1 64     = Just $ Ly.duration 64
    fn 1 128    = Just $ Ly.duration 128
    fn _ _      = Nothing
  


olyAccidental :: Accidental -> Maybe LyAccidental
olyAccidental Nat            = Nothing
olyAccidental Sharp          = Just sharp
olyAccidental Flat           = Just flat
olyAccidental DoubleSharp    = Just doubleSharp
olyAccidental DoubleFlat     = Just doubleFlat
     
olyOctaveSpec :: Int -> Maybe LyOctaveSpec
olyOctaveSpec i 
    | i > 0       = Just $ raised i
    | i < 0       = Just $ lowered (abs i)
    | otherwise   = Nothing 

-- we want to evalProcess on each strata so we always start from the 
-- same relative pitch and duration

{-
generateLilyPond :: LySystem -> Notate_Ly_Env -> LilyPondExprs
generateLilyPond sc env = error $ "generateLilyPond to do"
    -- evalNotate (renderSystem sc) state0 env
-}

translateLilyPond :: ScoreSystem -> Notate_Ly_Env -> LySystem
translateLilyPond (ScSystem se) env =
    ScSystem $ F.foldl fn mempty se
  where
    fn se e = se |> transStrata e env 
    
transStrata :: ScoreStrata -> Notate_Ly_Env -> LyStrata
transStrata s env = evalNotate (unwrapMonad $ changeRep s) state0 env 

changeRep :: ScoreStrata
          -> WrappedMonad (NotateM Notate_Ly_State Notate_Ly_Env) LyStrata
changeRep = traverse changeRepBody

changeRepBody :: ScoreGlyph 
              -> WrappedMonad (NotateM Notate_Ly_State Notate_Ly_Env) LilyPondGlyph
changeRepBody g = WrapMonad $ changeGlyph g

changeGlyph :: ScoreGlyph -> ProcessM LilyPondGlyph 
changeGlyph (GlyNote p d)       = 
    GlyNote   <$> changePitch p <*> chgDur d
    
changeGlyph (GlyRest d)         = GlyRest   <$> chgDur d

changeGlyph (GlySpacer d)       = GlySpacer <$> chgDur d

changeGlyph (GlyChord se d)     = 
    GlyChord <$> mapM changePitch se <*> chgDur d

changeGlyph (GlyGraceNotes se)  = GlyGraceNotes <$> mapM fn se
  where fn (p,d) = (,) <$> changePitch p <*> chgDur d


changePitch p@(Pitch l a o)     = fn <$> differOctaveSpec p
  where 
    fn os = LilyPondPitch (lyPitchName l) (olyAccidental a) os

chgDur d = LilyPondDuration <$> differDuration d

branchEq :: Eq a => a -> a -> b -> b -> b
branchEq a b eqk neqk = if a==b then eqk else neqk
                   
differDuration :: Duration -> ProcessM (Maybe LyDuration)
differDuration d = do
    old <- gets relative_duration
    branchEq d old same diff
  where 
    same = return Nothing  
    diff = do { modify (\s -> s {relative_duration = d})
              ; return $ olyDuration d}                              

differOctaveSpec :: Pitch -> ProcessM (Maybe LyOctaveSpec)
differOctaveSpec p = do
    old <- gets relative_pitch
    branchEq 0 (octaveDist old p) same (diff old p)
  where
    same       = return Nothing  
    diff old p = do { modify (\s -> s {relative_pitch = p})
                    ; return $ olyOctaveSpec $ octaveDist old p}  

    

lyPitchName :: PitchLetter -> LyPitchName
lyPitchName = toEnum . fromEnum 

lynote :: LyPitch -> Maybe LyDuration -> LyNote
lynote p od = note p *! od

lypitch :: LyPitchName -> Maybe LyAccidental -> Maybe LyOctaveSpec -> LyPitch
lypitch pn oa os = pitch pn *! oa *! os


--------------------------------------------------------------------------------
-- pretty printing

lyppopt :: Maybe (Ly a) -> PP.Doc
lyppopt (Just a) = unwrap a
lyppopt Nothing  = PP.empty

instance PP.Pretty LilyPondPitch where
  pretty (LilyPondPitch l oa os) = 
      PP.pretty l PP.<> lyppopt oa PP.<> lyppopt os
  
instance PP.Pretty LilyPondDuration where
  pretty (LilyPondDuration od)   = lyppopt od


  