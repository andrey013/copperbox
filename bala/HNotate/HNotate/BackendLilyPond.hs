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
import HNotate.NoteList
-- import HNotate.OutputUtils
import HNotate.Pitch
import HNotate.TextLilyPond hiding (chord, relative, rest, spacer)
import qualified HNotate.TextLilyPond as Ly
import HNotate.Traversals

import Control.Applicative
import Control.Monad.Identity
import Control.Monad.State
import qualified Data.Foldable as F
import Data.Monoid
import Data.Sequence


  
type LilyPondNoteList = LyCxt_Element


-- the relative pitch transformation needs something that the 
-- simplification doesn't account for
-- type LyPdGlyph = Glyph LyPitch (Maybe LyDuration)


--- change octave spec to infinity or (-1)





translateLilyPond :: ScoreNoteList -> Pitch -> LilyPondNoteList
translateLilyPond notes relative_pitch =
    outputNoteList $ lilypondForm notes relative_pitch



-- to do - look at fusing the traversals
lilypondForm :: ScoreNoteList -> Pitch -> ScoreNoteList
lilypondForm se relative_pitch = fn se  
  where
    fn se = let s'   = runLengthEncodeDuration se quarter
                s''  = traversalState pitch_conv_Body s' relative_pitch
            in s''

pitch_conv_Body :: ScoreGlyph
                -> WrappedMonad (State Pitch) ScoreGlyph
pitch_conv_Body e@(SgNote p _)      = WrapMonad $ do 
    nt <- convPitch p True 
    return $ changePitch e nt

pitch_conv_Body (SgRest d)          = WrapMonad $ return $ SgRest d
             
pitch_conv_Body (SgSpacer d)        = WrapMonad $ return $ SgSpacer d

pitch_conv_Body (SgChord se d)      = WrapMonad $ do
    se' <- F.foldlM (\a e -> (a |>) <$> convPitch e False) mempty se
    return $ SgChord se' d
    
pitch_conv_Body (SgGraceNotes se)   = WrapMonad $ do
    se' <- F.foldlM fn mempty se
    return $ SgGraceNotes se' 
  where
    fn a p = do 
        p' <- convPitch p False
        return $ a |> p'
    
     
        

convPitch :: Pitch -> Bool -> State Pitch Pitch
convPitch p update = do
    base <- get
    when (update==True) (put p)
    return $ modifyRelativeOctave base p


modifyRelativeOctave :: Pitch -> Pitch -> Pitch
modifyRelativeOctave base pch@(Pitch l a o) = 
    let dist = base `octaveDist` pch in Pitch l a dist
       

outputNoteList :: ScoreNoteList -> LilyPondNoteList
outputNoteList (ScNoteList se) = F.foldl outputBlock elementStart se

outputBlock :: LilyPondNoteList -> ScoreBlock -> LilyPondNoteList
outputBlock cxt (ScSingleBlock i se) =
    let barcheck = barNumber i
    in addBarline $ outputMeasure (cxt +++ barcheck) se

outputBlock cxt (ScPolyBlock i se) = 
    let barcheck = barNumber i
        voices = F.foldr (\e a -> (outputMeasure elementStart e) : a) [] se
    in polyphony (cxt +++ barcheck) voices 
    
outputMeasure :: LilyPondNoteList -> ScoreMeasure -> LilyPondNoteList
outputMeasure cxt (ScMeasure se) = F.foldl outputGlyph cxt se

outputGlyph :: LilyPondNoteList -> ScoreGlyph -> LilyPondNoteList
outputGlyph cxt (SgNote p d)        = cxt +++ mkLyNote p `durAttr` d

outputGlyph cxt (SgRest d)          = cxt +++ Ly.rest `durAttr` d

outputGlyph cxt (SgSpacer d)        = cxt +++ Ly.spacer `durAttr` d

outputGlyph cxt (SgChord se d)      = cxt +++ mkChord se `durAttr` d
  where 
    mkChord = Ly.chord . F.foldr (\e a -> (mkLyPitch e):a) []  

outputGlyph cxt (SgGraceNotes se)  = cxt +++ mkGrace se
  where 
    mkGrace = grace . F.foldl (\a p -> a +++ mkLyNote p) elementStart


polyphony :: LyCxt_Element -> [LyCxt_Element] -> LyCxt_Element
polyphony cxt (a:b:xs)  = polywork ((cxt +++ openPoly) `mappend` a) b xs
polyphony cxt [a]       = cxt `mappend` a
polyphony cxt []        = cxt


polywork cxt x []     = (cxt \\ x) +++ closePoly
polywork cxt x (y:ys) = polywork (cxt \\ x) y ys

durAttr g d | d == no_duration  = g
            | otherwise         = g ! mkLyDuration d   


mkLyNote :: Pitch -> LyNote
mkLyNote = Ly.note . mkLyPitch

mkLyPitch :: Pitch -> LyPitch
mkLyPitch (Pitch l a o) = 
    let pc = Ly.pitch $ lyPitchName l
        oa = olyAccidental a
        oo = olyOctaveSpec o
    in pc *! oa *! oo


lyPitchName :: PitchLetter -> Ly.LyPitchName
lyPitchName = toEnum . fromEnum 

olyAccidental :: Accidental -> Maybe Ly.LyAccidental
olyAccidental Nat            = Nothing
olyAccidental Sharp          = Just Ly.sharp
olyAccidental Flat           = Just Ly.flat
olyAccidental DoubleSharp    = Just Ly.doubleSharp
olyAccidental DoubleFlat     = Just Ly.doubleFlat
     
olyOctaveSpec :: Int -> Maybe Ly.LyOctaveSpec
olyOctaveSpec i 
    | i > 0       = Just $ Ly.raised i
    | i < 0       = Just $ Ly.lowered (abs i)
    | otherwise   = Nothing 
    
mkLyDuration :: Duration -> Ly.LyDuration
mkLyDuration drn = 
    let (n,d,dots) = durationElements drn in addDots dots $ mkDuration n d
  where 
    mkDuration 4 1      = Ly.longa  
    mkDuration 2 1      = Ly.breve 
    mkDuration 1 i      = Ly.duration i
    mkDuration n d      = error $ "olyDuration unhandled case - " ++ 
                                  show n ++ "%" ++ show d
    
    addDots :: Int -> Ly.LyDuration -> Ly.LyDuration
    addDots i d 
        | i > 0     = d ! (Ly.dotted i)
        | otherwise = d

barNumber :: Int -> LyBarNumberCheck
barNumber = barNumberCheck

addBarline :: LyCxt_Element -> LyCxt_Element
addBarline cxt = cxt +++ suffixLinebreak barcheck

