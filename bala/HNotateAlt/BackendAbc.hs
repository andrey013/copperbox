{-# LANGUAGE FlexibleContexts #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  BackendAbc
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Emit Abc from Score representation.
--
--------------------------------------------------------------------------------

module BackendAbc (
    Notate_Abc_Env(..), default_abc_env,
    generateAbc
  ) where

import CommonUtils
import Duration
import NotateMonad
import TextAbc
import Pitch
import ScoreRepresentation

{-
import HNotate.Backend.Abc.AbcFragments
import HNotate.Backend.Abc.AbcScoreDatatypes
import HNotate.Base.Datatypes
import HNotate.Base.NotateMonad
import HNotate.Print.Base
import HNotate.Print.OutputAbc
import HNotate.Score.Utils
-}

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Foldable as F
import Data.Monoid
import Data.Sequence
import qualified Data.Traversable as T
import qualified Text.PrettyPrint.Leijen as PP

newtype AbcExprs = AbcExprs { 
    getAbcExprs :: Seq AbcMusicLine
  }
  
type AbcMusicLine = AbcCxt_Body

instance PP.Pretty AbcExprs where
  pretty (AbcExprs se) = F.foldl fn PP.empty se
    where fn a e = a PP.<$> PP.text "____" PP.<$> getAbc e
    
    

type ProcessM a = NotateM Notate_Abc_State Notate_Abc_Env a

type AbcSystem    = ScSystem Glyph Duration
type AbcStrata    = ScStrata Glyph Duration
type AbcChunk     = ScChunk Glyph Duration
type AbcMeasure   = ScMeasure Glyph Duration
type AbcGlyph     = ScGlyph Glyph Duration

type Glyph      = CommonGlyph Duration

data Notate_Abc_State = Notate_Abc_State {
    abc_unknown_st      :: ()
  }
  deriving (Show)

data Notate_Abc_Env= Notate_Abc_Env {
    default_note_length      :: Duration
  }

state0 = Notate_Abc_State ()

default_abc_env :: Notate_Abc_Env
default_abc_env = Notate_Abc_Env {
    default_note_length        = semiquaver
  }

generateAbc :: AbcSystem -> Notate_Abc_Env -> AbcExprs
generateAbc sc env = evalNotate (renderSystem sc) state0 env




abcPitchLetter   :: Pitch -> AbcPitchLetter
abcPitchLetter = toEnum . fromEnum . pitch_letter



oabcAccidental :: Pitch -> Maybe AbcAccidental
oabcAccidental = fn . accidental
  where
    fn Nat            = Nothing
    fn Sharp          = Just sharp
    fn Flat           = Just flat
    fn DoubleSharp    = Just doubleSharp
    fn DoubleFlat     = Just doubleFlat


suffixWith :: (Append Abc cxts cxta, Monoid (Abc cxts))
           => Abc cxts
           -> ProcessM (Abc cxta)
           -> ProcessM (Abc cxts)
suffixWith ctx f = (ctx +++) <$> f



renderSystem (ScSystem se) = AbcExprs <$> T.mapM renderStrata se


renderStrata :: AbcStrata -> ProcessM AbcMusicLine
renderStrata (ScStrata i se) = F.foldlM renderMeasure body se



renderMeasure :: AbcCxt_Body -> AbcMeasure -> ProcessM AbcCxt_Body
renderMeasure cxt (ScMeasure i xs se) = (\mea -> cxt <+< mea +++ barline)
    <$> F.foldlM renderGlyph body se
    



-- Glyphs may generate elements of different types, hence we
-- return a 'suffix function' instead...
-- Also we have the very general return type ...(Abc cxts -> Abc cxts)
-- rather than ...EltS as we might need to think about beaming
-- which prints note without separating spaces at some point.
renderGlyph :: AbcCxt_Body -> AbcGlyph -> ProcessM AbcCxt_Body
renderGlyph cxt (ScGlyph gly d) = renderGly cxt gly d

renderGly cxt (CmnNote p) d             = suffixWith cxt $
    (*!) <$> renderPitch p <*> abcDuration d

renderGly cxt (CmnRest) d               = suffixWith cxt $
    (rest *!) <$> abcDuration d

renderGly cxt (CmnSpacer) d             = suffixWith cxt $
    (spacer *!) <$> abcDuration d


-- Notes inside chords have duration (though they all _should_ be the same)
renderGly cxt (CmnChord se) d           = suffixWith cxt $
    chord <$> F.foldrM (pitch1 d) [] se

  where
    pitch1 d scp acc  = fn acc <$> renderPitch scp <*> abcDuration d

    fn acc p od = p *! od : acc


renderGly cxt (CmnGraceNotes se) d        = suffixWith cxt $
    gracenotes <$> mapM graceNote (F.toList se)



graceNote :: (Pitch,Duration) -> ProcessM AbcNote
graceNote (scp,d)  = (*!) <$> renderPitch scp  <*> abcDuration d



-- | @AbcScPitch --> AbcNote@
renderPitch :: Pitch -> ProcessM AbcNote
renderPitch pch =
    fn <$> pure (abcPitchLetter pch) <*> pure (oabcAccidental pch)
  where
    fn pn oa = oa !*> (note pn)

abcDuration :: Duration -> ProcessM (Maybe AbcDuration)
abcDuration d = fn d <$> asks default_note_length
  where
    fn dur1 deft
      | dur1 == deft  = Nothing
      | otherwise     = Nothing
{-
      | otherwise     = let scale = denominator (toRatio deft)
                            r     = toRatio dur1
                            (n,d) = (numerator r, denominator r)
                        in Just $ dur ( n*scale, d)

-}                        