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

import HNotate.Backend.LilyPond.LyFragments
import HNotate.Backend.LilyPond.LyScoreDatatypes
import HNotate.Base.Datatypes
import HNotate.Base.NotateMonad
import HNotate.Print.OutputLilyPond
import HNotate.System.SystemLilyPond

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Foldable as F
import Data.List (maximumBy)
import Data.Maybe (catMaybes)
import Data.Monoid
import Data.Ratio
import Data.Sequence hiding (reverse)
import qualified Data.Traversable as T


type ProcessM a = NotateM Notate_Ly_State Notate_Ly_Env a

data Notate_Ly_State = Notate_Ly_State {
    relative_pitch      :: Pitch,
    relative_duration   :: Duration
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


generateLilyPondScore :: LyScScore -> Notate_Ly_Env -> PartLyExprs
generateLilyPondScore sc env = evalNotate (renderScore sc) state0 env



olyDuration :: Duration -> Maybe LyDuration
olyDuration = fn . getDuration
  where 
    fn (4,1)      = Just longa  
    fn (2,1)      = Just breve 
    fn (1,1)      = Just $ duration 1
    fn (1,2)      = Just $ duration 2
    fn (1,4)      = Just $ duration 4
    fn (1,8)      = Just $ duration 8
    fn (1,16)     = Just $ duration 16
    fn (1,32)     = Just $ duration 32
    fn (1,64)     = Just $ duration 64
    fn (1,128)    = Just $ duration 128
    fn _          = Nothing
  
  
olyAccidental :: Pitch -> Maybe LyAccidental
olyAccidental = fn . accidental
  where
    fn Nat            = Nothing
    fn Sharp          = Just sharp
    fn Flat           = Just flat
    fn DoubleSharp    = Just doubleSharp
    fn DoubleFlat     = Just doubleFlat
    
olyOctaveSpec :: Int -> Maybe LyOctaveSpec
olyOctaveSpec i 
    | i > 0       = Just $ raised i
    | i < 0       = Just $ lowered (abs i)
    | otherwise   = Nothing 

                      
                          

lyPitchName :: Pitch -> LyPitchName
lyPitchName = toEnum . fromEnum . pitch_letter

lynote :: LyPitch -> Maybe LyDuration -> LyNote
lynote p od = note p *! od

lypitch :: LyPitchName -> Maybe LyAccidental -> Maybe LyOctaveSpec -> LyPitch
lypitch pn oa os = pitch pn *! oa *! os


suffixWith :: (Append Ly cxts cxta, Monoid (Ly cxts))
           => Ly cxts
           -> ProcessM (Ly cxta)
           -> ProcessM (Ly cxts)
suffixWith ctx f = (ctx +++) <$> f




-- | @LyScScore --> \\book@
renderScore :: LyScScore -> ProcessM PartLyExprs
renderScore (LyScScore se) = T.mapM renderPart se



-- | @LyScPart --> \\score@
renderPart :: LyScPart -> ProcessM PartLyMusicExpr
renderPart (LyScPart i se) =
    F.foldlM renderPolyPhrase elementStart se



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
renderMeasure cxt (LyScMeasure i xs se) = (\mea -> cxt <+< mea +++ barcheck)
    <$> F.foldlM renderGlyph elementStart se



renderGlyph :: LyCxt_Element -> LyScGlyph -> ProcessM LyCxt_Element
renderGlyph cxt (LyScNote scp d)            = suffixWith cxt $
    lynote <$> renderPitch scp  <*> differDuration d


renderGlyph cxt (LyScRest d)                = suffixWith cxt $
    (rest *!)   <$> differDuration d

renderGlyph cxt (LyScSpacer d)              = suffixWith cxt $
    (spacer *!) <$> differDuration d

-- Important: successive notes in a chord shouldn't change relative pitch

renderGlyph cxt a@(LyScChord se d)          = suffixWith cxt $
    case viewl se of
      EmptyL      -> error "Empty chord"
      (e :< sse)  -> do 
          x <- renderPitch e
          xs <-  mapM renderPitch2 (F.toList sse)
          od <- differDuration d
          return (chord (x:xs) *! od)

    

renderGlyph cxt (LyScGraceNotes se)         = suffixWith cxt $
    (\xs -> grace $ blockS $ foldl (+++) elementStart xs) 
        <$> mapM renderGrace (F.toList se)



renderGrace :: (Pitch,Duration) -> ProcessM LyNote
renderGrace (pch,dur) =
    lynote <$> renderPitch pch  <*> differDuration dur



-- | @LyScPitch --> LyPitch@
renderPitch :: Pitch -> ProcessM LyPitch
renderPitch pch =
    lypitch <$> pure (lyPitchName pch) <*> pure (olyAccidental pch)
                                       <*> differOctaveSpec pch


renderPitch2 :: Pitch -> ProcessM LyPitch
renderPitch2 pch = return $ 
    lypitch (lyPitchName pch) (olyAccidental pch) Nothing


differOctaveSpec :: Pitch -> ProcessM (Maybe LyOctaveSpec)
differOctaveSpec p = (\old -> olyOctaveSpec $ octaveDist old p)
    <$> gets relative_pitch <* modify (\s -> s {relative_pitch = p})



differDuration :: Duration -> ProcessM (Maybe LyDuration)
differDuration d = (\old -> if (old==d) then Nothing else olyDuration d)
    <$> gets relative_duration <* modify (\s -> s {relative_duration = d})






