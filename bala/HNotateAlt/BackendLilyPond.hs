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
    Notate_Ly_Env(..), default_ly_env,
    generateLilyPond
  ) where


import CommonUtils
import Duration hiding (breve,longa)
import TextLilyPond
import NotateMonad
import Pitch
import ScoreRepresentation

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Foldable as F
import Data.Monoid
import Data.Sequence hiding (reverse)
import qualified Data.Traversable as T
import qualified Text.PrettyPrint.Leijen as PP

newtype LilyPondExprs = LilyPondExprs { 
    getLilyPondExprs :: Seq LilyPondMusicLine
  }
  
type LilyPondMusicLine = LyCxt_Element

instance PP.Pretty LilyPondExprs where
  pretty (LilyPondExprs se) = F.foldl fn PP.empty se
    where fn a e = a PP.<$> PP.text "____" PP.<$> getLy e



type ProcessM a = NotateM Notate_Ly_State Notate_Ly_Env a

type LySystem    = ScSystem Glyph Duration
type LyStrata    = ScStrata Glyph Duration
type LyBlock     = ScBlock Glyph Duration
type LyMeasure   = ScMeasure Glyph Duration
type LyGlyph     = ScGlyph Glyph Duration



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


generateLilyPond :: LySystem -> Notate_Ly_Env -> LilyPondExprs
generateLilyPond sc env = evalNotate (renderSystem sc) state0 env



olyDuration :: Duration -> Maybe LyDuration
olyDuration d = let (i,dots) = getDuration d in fn i
  where 
    fn (-4)       = Just longa  
    fn (-2)       = Just breve 
    fn 1          = Just $ duration 1
    fn 2          = Just $ duration 2
    fn 4          = Just $ duration 4
    fn 8          = Just $ duration 8
    fn 16         = Just $ duration 16
    fn 32         = Just $ duration 32
    fn 64         = Just $ duration 64
    fn 128        = Just $ duration 128
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
renderSystem :: LySystem -> ProcessM LilyPondExprs
renderSystem (ScSystem se) = LilyPondExprs <$> T.mapM renderStrata se



-- | @LyScPart --> \\score@
renderStrata :: LyStrata -> ProcessM LilyPondMusicLine
renderStrata (ScStrata i se) = 
    undefined
{-    
  -- NOT CORRECT - poly!
    F.foldlM renderMeasure elementStart se
-}

{-
renderPolyPhrase :: LyCxt_Element -> LyScPolyPhrase -> ProcessM LyCxt_Element
renderPolyPhrase cxt (LyScSingletonPhrase x)   =
    (cxt `mappend`) <$> renderSegment elementStart x

renderPolyPhrase cxt (LyScPolyPhrase xs)   =
    mergePolys cxt <$> mapM (renderSegment elementStart) xs

-}

mergePolys k (x:xs) = let poly = foldl fn (block x) xs in
    k +++ openPoly +++ poly +++ closePoly
  where
    fn acc a = acc \\ (block a)
mergePolys k _      = error "mergePolys - ill constructed PolyPhrase - a bug"





renderMeasure :: LyCxt_Element -> LyMeasure -> ProcessM LyCxt_Element
renderMeasure cxt (ScMeasure se) = (\mea -> cxt <+< mea +++ barcheck)
    <$> F.foldlM renderGlyph elementStart se



renderGlyph :: LyCxt_Element -> LyGlyph -> ProcessM LyCxt_Element
renderGlyph cxt (ScGlyph gly d) = renderGly cxt gly d

renderGly cxt (CmnNote p) d             = suffixWith cxt $
    lynote <$> renderPitch p <*> differDuration d

renderGly cxt (CmnRest) d               = suffixWith cxt $
    (rest *!)   <$> differDuration d

renderGly cxt (CmnSpacer) d             = suffixWith cxt $
    (spacer *!) <$> differDuration d

-- Important: successive notes in a chord shouldn't change relative pitch
renderGly cxt (CmnChord se) d           = suffixWith cxt $
    case viewl se of
      EmptyL      -> error "Empty chord"
      (e :< sse)  -> do 
          x <- renderPitch e
          xs <-  mapM renderPitch2 (F.toList sse)
          od <- differDuration d
          return (chord (x:xs) *! od)

renderGly cxt (CmnGraceNotes se) d        = suffixWith cxt $
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






