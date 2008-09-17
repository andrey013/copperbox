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
    LilyPondOutput,
  ) where


import HNotate.CommonUtils
import HNotate.Duration
import HNotate.NoteList
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
import Data.Traversable


  
type LilyPondOutput = LyCxt_Element



translateLilyPond :: ScoreNoteList -> Pitch -> LilyPondOutput
translateLilyPond notes relative_pitch =
    outputNoteList $ lilypondRelativeForm notes relative_pitch


-- Do we need a state type, like this one?
-- data LyState = LyState { rel_pitch :: Pitch, rel_dur :: Duration }


lilypondRelativeForm :: ScoreNoteList -> Pitch -> ScoreNoteList
lilypondRelativeForm se relative_pitch = 
    evalState (unwrapMonad $ inner se) relative_pitch 
  where       
    inner se = evalState (unwrapMonad $ unComp $ trav se) quarter 
    trav     = traverse (proBody `comp` drleBody)

outputNoteList :: ScoreNoteList -> LilyPondOutput
outputNoteList (ScNoteList se) = F.foldl outputBlock elementStart se

outputBlock :: LilyPondOutput -> ScoreBlock -> LilyPondOutput
outputBlock cxt (ScSingleBlock i se) =
    let barcheck = barNumber i
    in addBarline $ outputMeasure (cxt +++ barcheck) se

outputBlock cxt (ScPolyBlock i se) = 
    let barcheck = barNumber i
        voices = F.foldr (\e a -> (outputMeasure elementStart e) : a) [] se
    in polyphony (cxt +++ barcheck) voices 
    
outputMeasure :: LilyPondOutput -> ScoreMeasure -> LilyPondOutput
outputMeasure cxt (ScMeasure se) = F.foldl outputGlyph cxt se

outputGlyph :: LilyPondOutput -> ScoreGlyph -> LilyPondOutput
outputGlyph cxt (SgNote p d)        = cxt +++ mkLyNote p `durAttr` d

outputGlyph cxt (SgRest d)          = cxt +++ Ly.rest `durAttr` d

outputGlyph cxt (SgSpacer d)        = cxt +++ Ly.spacer `durAttr` d

outputGlyph cxt (SgChord se d)      = cxt +++ mkChord se `durAttr` d
  where 
    mkChord = Ly.chord . F.foldr (\e a -> (mkLyPitch e):a) []  

outputGlyph cxt (SgGraceNotes se)  = cxt +++ mkGrace se
  where 
    mkGrace = grace . block . F.foldl (\a p -> a +++ mkLyNote p) elementStart


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

