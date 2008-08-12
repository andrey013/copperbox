{-# LANGUAGE FlexibleContexts #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  HNotate.Backend.Abc.AbcBackend
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Emit Abc from AbcScore.
--
--------------------------------------------------------------------------------

module HNotate.Backend.Abc.AbcBackend (
    Notate_Abc_Env(..), default_abc_env,
    generateAbcScore
  ) where

import HNotate.Backend.Abc.AbcFragments
import HNotate.Backend.Abc.AbcScoreDatatypes
import HNotate.Base.Datatypes
import HNotate.Base.NotateMonad
import HNotate.Print.Base
import HNotate.Print.OutputAbc
import HNotate.Score.Utils

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Foldable as F
import Data.Maybe (catMaybes)
import Data.Monoid
import Data.Ratio
import Data.Sequence


type ProcessM a = NotateM Notate_Abc_State Notate_Abc_Env a



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
    default_note_length        = quarternote
  }


infixl 7 *!
(*!) e oa   = maybe e (e !) oa

infixl 7 !*>
(!*>) oa e   = maybe e ((flip (!>)) e) oa


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



type EltS = AbcCxt_Body -> AbcCxt_Body


suffix :: (Append Abc cxts cxta, Monoid (Abc cxts)) => Abc cxta -> (Abc cxts -> Abc cxts)
suffix = flip (+++)

suffixA :: (Append Abc cxts cxta, Monoid (Abc cxts), Functor f)
        => f (Abc cxta)
        -> f (Abc cxts -> Abc cxts)
suffixA f = suffix <$> f

concatS :: [a -> a] -> a -> a
concatS = foldr ( #. ) id

-- foldlOp :: (Functor f) => (t -> f (b -> c)) -> (a -> b) -> t -> f (a -> c)
foldlOpA f op = \acc e -> (acc `op`) <$> f e


-- first step get the original Abc rendering working
-- then worry if we should return a function or not
generateAbcScore :: AbcScTuneBook -> Notate_Abc_Env -> PartAbcExprs
generateAbcScore sc env = evalNotate (renderTuneBook sc) abc_state env
  where
    abc_state = state0


renderTuneBook (AbcScTuneBook se) = F.foldlM fn mempty se
  where
    fn xs p = (xs |>) <$> renderTune p
 --    foldlM (foldlOpA renderTune (flip (:))) m se

renderTune :: AbcScTune -> ProcessM PartAbcMusicExpr
renderTune (AbcScTune i se) = F.foldlM renderPolyPhrase body se


renderPolyPhrase :: AbcCxt_Body -> AbcScPolyPhrase -> ProcessM AbcCxt_Body
renderPolyPhrase cxt (AbcScSingletonPhrase x) = ( cxt # ) <$> renderMeasure x
renderPolyPhrase cxt (AbcScPolyPhrase xs) =
    error "Abc renderPolyPhrase - to do" -- foldlM (foldlOpA renderMeasure (flip ( #. ))) id xs



renderMeasure :: (Append Abc cxts AbcGraceNotesT,
                  Append Abc cxts AbcChordT,
                  Append Abc cxts AbcNoteT,
                  Append Abc cxts AbcRestT,
                  Monoid (Abc cxts))
              => AbcScMeasure
              -> ProcessM (Abc cxts -> Abc cxts)

renderMeasure (AbcScMeasure i xs se) = 
    F.foldlM (foldlOpA renderGlyph ( #. )) id se




-- Glyphs may generate elements of different types, hence we
-- return a 'suffix function' instead...
-- Also we have the very general return type ...(Abc cxts -> Abc cxts)
-- rather than ...EltS as we might need to think about beaming
-- which prints note without separating spaces at some point.
renderGlyph :: (Append Abc cxts AbcGraceNotesT,
                Append Abc cxts AbcChordT,
                Append Abc cxts AbcNoteT,
                Append Abc cxts AbcRestT,
                Monoid (Abc cxts))
            => AbcScGlyph
            -> ProcessM (Abc cxts -> Abc cxts)

renderGlyph (AbcScNote scp d)            = suffixA $
    (*!) <$> renderPitch scp  <*> abcDuration d



renderGlyph (AbcScRest d)                = suffixA $
    (rest *!)   <$> abcDuration d

renderGlyph (AbcScSpacer d)               = suffixA $
    (spacer *!) <$> abcDuration d


-- Notes inside chords have duration (though they all _should_ be the same)
renderGlyph (AbcScChord xs dur)               = suffixA $
    chord <$> F.foldrM (pitch1 dur) [] xs

  where
    pitch1 d scp acc  = fn acc <$> renderPitch scp <*> abcDuration d

    fn acc p od = p *! od : acc


renderGlyph (AbcScGraceNotes se)          = suffixA $
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
      | otherwise     = let scale = denominator (toRatio deft)
                            r     = toRatio dur1
                            (n,d) = (numerator r, denominator r)
                        in Just $ dur ( n*scale, d)

                        