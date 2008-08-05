{-# LANGUAGE FlexibleContexts #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  HNotate.Backend.LilyPond.LyBackend
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Emit LilyPond from LyScore.
--
--------------------------------------------------------------------------------

module HNotate.Backend.LilyPond.LyBackend (
    Notate_Ly_Env(..), default_ly_env,
    generateLilyPondScore
  ) where


import HNotate.Backend.LilyPond.LyScoreDatatypes
import HNotate.Base.Datatypes
import HNotate.Base.NotateMonad
import HNotate.Print.OutputLilyPond


import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Foldable as F
import Data.List (maximumBy)
import Data.Maybe (catMaybes)
import Data.Monoid
import Data.Ratio
import Data.Sequence hiding (reverse)


type ProcessM a = NotateM Notate_Ly_State Notate_Ly_Env a

data Notate_Ly_State = Notate_Ly_State {
    relative_pitch      :: Pitch,
    relative_duration   :: Duration
    -- part_refs           :: ScPolyRefs pch dur
  }


data Notate_Ly_Env = Notate_Ly_Env {
    initial_ly_context        :: LyCxt_Element,
    initial_relative_pitch    :: Pitch
  }



infixl 7 *!
(*!) e oa   = maybe e (e !) oa


state0 :: Notate_Ly_State
state0 = Notate_Ly_State {
    relative_pitch      = middleC,
    relative_duration   = quarternote
  }

default_ly_env :: Notate_Ly_Env
default_ly_env = Notate_Ly_Env {
    initial_ly_context        = elementStart,
    initial_relative_pitch    = middleC
  }


generateLilyPondScore :: LyScScore -> Notate_Ly_Env -> [LyCmdScore]
generateLilyPondScore sc env = evalNotate (renderScore sc) ly_state env
  where
    ly_state = state0



getPitch (LyScNote scp _) = scp


olyDuration :: Duration -> Maybe LyDuration
olyDuration _ = Just $ duration 4

olyAccidental :: Pitch -> Maybe LyAccidental
olyAccidental = fn . accidental
  where
    fn Nat            = Nothing
    fn Sharp          = Just sharp
    fn Flat           = Just flat
    fn DoubleSharp    = Just doubleSharp
    fn DoubleFlat     = Just doubleFlat

lyPitchName :: Pitch -> LyPitchName
lyPitchName = toEnum . fromEnum . pitch_letter


suffixWith :: (Append Ly cxts cxta, Monoid (Ly cxts))
           => Ly cxts
           -> ProcessM (Ly cxta)
           -> ProcessM (Ly cxts)
suffixWith ctx f = (ctx +++) <$> f




-- | @LyScScore --> \\book@
renderScore :: LyScScore -> ProcessM [LyCmdScore]
renderScore (LyScScore se) = F.foldlM fn [] se
  where
    fn xs p = flip (:) xs <$> renderPart p



-- | @LyScPart --> \\score@
renderPart :: LyScPart -> ProcessM LyCmdScore
renderPart (LyScPart i se) =
    (score . block) <$> F.foldlM renderPolyPhrase elementStart se



renderPolyPhrase :: LyCxt_Element -> LyScPolyPhrase -> ProcessM LyCxt_Element
renderPolyPhrase cxt (LyScSingletonPhrase x)   =
    (cxt `mappend`) <$> renderSegment elementStart x

renderPolyPhrase cxt (LyScPolyPhrase xs)   =
    mergePolys cxt <$> mapM (renderSegment elementStart) xs



mergePolys k (x:xs) = let poly = foldl fn (block x) xs in
    k +++ openPoly +++ poly +++ closePoly
  where
    fn acc a = acc \\ (block a)
mergePolys k _      = error "mergePolys - ill constructed PolyPhrase - a bug"


renderSegment :: LyCxt_Element -> LyScSegment -> ProcessM LyCxt_Element
renderSegment cxt (LyScSegment se) = F.foldlM renderMeasure cxt se



renderMeasure :: LyCxt_Element -> LyScMeasure -> ProcessM LyCxt_Element
renderMeasure cxt (LyScMeasure i xs se) = F.foldlM renderGlyph cxt se



renderGlyph :: LyCxt_Element -> LyScGlyph -> ProcessM LyCxt_Element
renderGlyph cxt (LyScNote scp d)            = suffixWith cxt $
    fn <$> renderPitch scp  <*> differDuration d
  where
    fn p od = note p *! od

renderGlyph cxt (LyScRest d)                = suffixWith cxt $
    (rest *!)   <$> differDuration d

renderGlyph cxt (LyScSpacer d)              = suffixWith cxt $
    (spacer *!) <$> differDuration d

renderGlyph cxt a@(LyScChord xs d)          = suffixWith cxt $
    fn <$> mapM renderPitch xs
       <*> differDuration d
  where
    fn xs od = chord xs *! od

renderGlyph cxt (LyScGraceNotes xs)         = suffixWith cxt $
    fn <$> mapM renderGrace xs
  where
    fn        = grace . blockS . foldl (+++) elementStart


renderGrace :: (Pitch,Duration) -> ProcessM LyNote
renderGrace (pch,dur) =
    fn <$> renderPitch pch  <*> differDuration dur
  where
    fn p od = note p *! od


-- | @LyScPitch --> LyPitch@
renderPitch :: Pitch -> ProcessM LyPitch
renderPitch pch =
    fn <$> pure (lyPitchName pch) <*> pure (olyAccidental pch)
                                  <*> differOctaveSpec pch
  where
    fn pn oa oos = (pitch pn) *! oa *! oos



differOctaveSpec :: Pitch -> ProcessM (Maybe LyOctaveSpec)
differOctaveSpec p = fn p <$> gets relative_pitch <*
                              modify (\s -> s {relative_pitch = p})
  where
    fn new old = let i = octaveDist old new
                 in case i `compare` 0 of
                      EQ -> Nothing
                      LT -> Just $ lowered (abs i)
                      GT -> Just $ raised i


differDuration :: Duration -> ProcessM (Maybe LyDuration)
differDuration d = fn d =<< gets relative_duration
  where
    fn new old | new == old   = return Nothing
               | otherwise    = do
                      modify (\s -> s {relative_duration = new})
                      return $ olyDuration new




