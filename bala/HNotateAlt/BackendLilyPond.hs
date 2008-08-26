{-# LANGUAGE TypeSynonymInstances #-}

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
    translateLilyPond, LyExprs(..),  LyMusicLine   
  ) where


import CommonUtils
import Duration
import OutputUtils
import Pitch
import ScoreRepresentation
import TextLilyPond
import Traversals

import Control.Applicative
import Control.Monad.Identity
import Control.Monad.State
import qualified Data.Foldable as F
import Data.Monoid
import Data.Sequence
import qualified Text.PrettyPrint.Leijen as PP

newtype LyExprs = LyExprs { 
    getLilyPondExprs :: Seq LyMusicLine
  }
  
type LyMusicLine = LyCxt_Element

instance PP.Pretty LyExprs where
  pretty (LyExprs se) = F.foldl fn PP.empty se
    where fn a e = a PP.<$> PP.text "____" PP.<$> getLy e


type LyPdGlyph = Glyph LyPitch (Maybe LyDuration)
                
                 
type LyPdSystem    = ScSystem   LyPdGlyph
type LyPdStrata    = ScStrata   LyPdGlyph
type LyPdBlock     = ScBlock    LyPdGlyph
type LyPdMeasure   = ScMeasure  LyPdGlyph




translateLilyPond :: ScoreSystem -> Pitch -> LyExprs
translateLilyPond sys relative_pitch =
    let (ScSystem se) = lilypondForm sys relative_pitch
    in LyExprs $ F.foldl fn mempty se
  where
    fn se e = se |> outputStrata e 

-- to do - look at fusing the traversals
lilypondForm :: ScoreSystem -> Pitch -> LyPdSystem
lilypondForm (ScSystem se) relative_pitch =
    ScSystem $ F.foldl fn mempty se  
  where
    fn se s  = let s'   = runLengthEncodeDuration s quarter
                   s''  = traversalIdentity ly_duration_Body s'
                   s''' = traversalState pitch_conv_Body s'' relative_pitch
              in se |> s'''

pitch_conv_Body :: Glyph Pitch d 
                -> WrappedMonad (State Pitch) (Glyph LyPitch d)
pitch_conv_Body e@(GlyNote p _)     = WrapMonad $ do 
    nt <- convPitch p True 
    return $ changePitch e nt

pitch_conv_Body (GlyRest d)         = WrapMonad $ return $ GlyRest d
             
pitch_conv_Body (GlySpacer d)       = WrapMonad $ return $ GlySpacer d

pitch_conv_Body (GlyChord se d)     = WrapMonad $ do
    se' <- F.foldlM (\a e -> (a |>) <$> convPitch e False) mempty se
    return $ GlyChord se' d
    
pitch_conv_Body (GlyGraceNotes se)  = WrapMonad $ do
    se' <- F.foldlM fn mempty se
    return $ GlyGraceNotes se' 
  where
    fn a (p,d) = do 
        p' <- convPitch p False
        return $ a |> (p',d)
    
     
        

convPitch :: Pitch -> Bool -> State Pitch LyPitch
convPitch p update = do
    base <- get
    when (update==True) (put p)
    return (buildNote base p)
  where
    buildNote base pch = (lymodpitch p) *! (lyRelativeOctave base p)
   

ly_duration_Body :: Glyph p (Maybe Duration) 
                 -> Identity (Glyph p (Maybe LyDuration))
ly_duration_Body e = let drn = glyphDuration e in
  case drn of
    Just d -> return $ changeDuration e (olyDuration d)
    Nothing -> return $ changeDuration e Nothing
                

outputStrata :: LyPdStrata -> LyMusicLine
outputStrata (ScStrata i se) = F.foldl outputBlock elementStart se

outputBlock :: LyMusicLine -> LyPdBlock -> LyMusicLine
outputBlock cxt (ScSingleBlock i se) = 
    (outputMeasure cxt se)

outputBlock cxt (ScPolyBlock i se) = 
    let voices = F.foldr (\e a -> (outputMeasure elementStart e) : a) [] se
    in polyphony cxt voices 
    
outputMeasure :: LyMusicLine -> LyPdMeasure -> LyMusicLine
outputMeasure cxt (ScMeasure se) = F.foldl outputGlyph cxt se

outputGlyph :: LyMusicLine -> LyPdGlyph -> LyMusicLine
outputGlyph cxt (GlyNote p od)      = cxt +++ note p *! od

outputGlyph cxt (GlyRest od)        = cxt +++ rest *! od

outputGlyph cxt (GlySpacer od)      = cxt +++ spacer *! od

outputGlyph cxt (GlyChord se od)    = cxt +++ mkChord se *! od
  where 
    mkChord = chord . F.foldr (:) []  

outputGlyph cxt (GlyGraceNotes se)  = cxt +++ mkGrace se
  where 
    mkGrace = grace . F.foldl (\a (p,od) -> a +++ note p *! od) elementStart


polyphony :: LyCxt_Element -> [LyCxt_Element] -> LyCxt_Element
polyphony cxt (a:b:xs)  = polywork ((cxt +++ openPoly) `mappend` a) b xs
polyphony cxt [a]       = cxt `mappend` a
polyphony cxt []        = cxt


polywork cxt x []     = (cxt \\ x) +++ closePoly
polywork cxt x (y:ys) = polywork (cxt \\ x) y ys
   



instance PP.Pretty LyDuration where
  pretty = getLy

instance PP.Pretty LyNote where
  pretty = getLy

