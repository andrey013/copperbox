{-# LANGUAGE TypeSynonymInstances #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  HNotate.BackendLilyPond
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

module HNotate.BackendLilyPond (
    translateLilyPond, 
    LilyPondNoteList,
   
     
  ) where


import HNotate.CommonUtils
import HNotate.Duration
import HNotate.NoteListDatatypes
import HNotate.OutputUtils
import HNotate.Pitch
import HNotate.TextLilyPond hiding (relative)
import HNotate.Traversals

import Control.Applicative
import Control.Monad.Identity
import Control.Monad.State
import qualified Data.Foldable as F
import Data.Monoid
import Data.Sequence
import qualified Text.PrettyPrint.Leijen as PP

  
type LilyPondNoteList = LyCxt_Element



type LyPdGlyph = Glyph LyPitch (Maybe LyDuration)
                

type LyPdNoteList  = ScNoteList   LyPdGlyph
type LyPdBlock     = ScBlock    LyPdGlyph
type LyPdMeasure   = ScMeasure  LyPdGlyph




translateLilyPond :: ScoreNoteList -> Pitch -> LilyPondNoteList
translateLilyPond notes relative_pitch =
    outputNoteList $ lilypondForm notes relative_pitch



-- to do - look at fusing the traversals
lilypondForm :: ScoreNoteList -> Pitch -> LyPdNoteList
lilypondForm se relative_pitch = fn se  
  where
    fn se = let s'   = runLengthEncodeDuration se quarter
                s''  = traversalIdentity ly_duration_Body s'
                s''' = traversalState pitch_conv_Body s'' relative_pitch
            in s'''

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
                

outputNoteList :: LyPdNoteList -> LilyPondNoteList
outputNoteList (ScNoteList se) = F.foldl outputBlock elementStart se

outputBlock :: LilyPondNoteList -> LyPdBlock -> LilyPondNoteList
outputBlock cxt (ScSingleBlock i se) = 
    (outputMeasure cxt se)

outputBlock cxt (ScPolyBlock i se) = 
    let voices = F.foldr (\e a -> (outputMeasure elementStart e) : a) [] se
    in polyphony cxt voices 
    
outputMeasure :: LilyPondNoteList -> LyPdMeasure -> LilyPondNoteList
outputMeasure cxt (ScMeasure se) = F.foldl outputGlyph cxt se

outputGlyph :: LilyPondNoteList -> LyPdGlyph -> LilyPondNoteList
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
   

instance PP.Pretty LilyPondNoteList where
  pretty = getLy

instance PP.Pretty LyDuration where
  pretty = getLy

instance PP.Pretty LyNote where
  pretty = getLy

