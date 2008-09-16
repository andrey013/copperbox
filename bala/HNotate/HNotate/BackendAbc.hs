{-# LANGUAGE TypeSynonymInstances #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  HNotate.BackendAbc
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


module HNotate.BackendAbc (
    translateAbc, AbcNoteList   
  ) where

import HNotate.CommonUtils
import HNotate.Duration
import HNotate.Env
import HNotate.NoteList
import HNotate.Pitch
import HNotate.TextAbc hiding (gracenotes, chord, spacer, rest)
import qualified HNotate.TextAbc as Abc
import HNotate.Traversals

import Control.Applicative
import Control.Monad.Reader
import qualified Data.Foldable as F
import Data.Monoid
import Data.Ratio
import Data.Sequence hiding (take)
import Data.Traversable

  
type AbcNoteList = AbcCxt_Body


translateAbc :: ScoreNoteList -> Env -> AbcNoteList
translateAbc notes env =
    outputNoteList $ abcForm notes env

    
abcForm :: ScoreNoteList -> Env -> ScoreNoteList
abcForm se env = runReader (unwrapMonad $ traverse unleBody se) env 
 

outputNoteList :: ScoreNoteList -> AbcNoteList
outputNoteList (ScNoteList se) = F.foldl outputBlock body se

outputBlock :: AbcNoteList -> ScoreBlock -> AbcNoteList
outputBlock cxt (ScSingleBlock i se) = 
    let barcheck = barNumber i
    in  (outputMeasure (cxt +++ barcheck) se) +++ (suffixLinebreak barline)

outputBlock cxt (ScPolyBlock i se) = 
    let barcheck = barNumber i
        voices = F.foldr (\e a -> outputMeasure body e : a) [] se
    in voiceOverlay (cxt +++ barcheck) voices 

outputMeasure :: AbcNoteList -> ScoreMeasure -> AbcNoteList
outputMeasure cxt (ScMeasure se) = F.foldl outputGlyph cxt se

outputGlyph :: AbcNoteList -> ScoreGlyph -> AbcNoteList
outputGlyph cxt (SgNote n d)          = cxt +++ (mkAbcNote n `durAttr` d)
    
outputGlyph cxt (SgRest d)            = cxt +++ (Abc.rest `durAttr` d) 
    
outputGlyph cxt (SgSpacer d)          = cxt +++ (Abc.spacer `durAttr` d)
    
outputGlyph cxt (SgChord se d)        = cxt +++ mkChord d se
  where
    mkChord d = Abc.chord . F.foldr (\n a -> (mkAbcNote n `durAttr` d) : a) []
    
outputGlyph cxt (SgGraceNotes se)  = cxt +++ mkGrace se
  where 
    mkGrace = Abc.gracenotes . F.foldr (\n a -> mkAbcNote n : a) []

voiceOverlay :: AbcCxt_Body -> [AbcCxt_Body] -> AbcCxt_Body
voiceOverlay cxt []     = cxt
voiceOverlay cxt [v]    = cxt `mappend` v +++ barline
voiceOverlay cxt (v:vs) = voiceOverlay (cxt &\ v) vs 


barNumber :: Int -> AbcRemark
barNumber = remark . show

durAttr g d | d == durationZero = g
            | otherwise         = g ! mkAbcDuration d



mkAbcNote :: Pitch -> AbcNote
mkAbcNote (Pitch l a o) = 
    let pl = if o > 4 then upwards $ abcPitchLetter l else abcPitchLetter l
    in (oabcAccidental a !*> Abc.note pl *! oabcOve o)


abcPitchLetter :: PitchLetter -> Abc.AbcPitchLetter
abcPitchLetter = toEnum . fromEnum

upwards :: Abc.AbcPitchLetter -> Abc.AbcPitchLetter
upwards l = let i = fromEnum l in 
  if (i<7) then toEnum $ i + 7 else l


oabcAccidental :: Accidental -> Maybe Abc.AbcAccidental
oabcAccidental Nat          = Nothing
oabcAccidental a            = Just $ abcAccidental a

abcAccidental :: Accidental -> Abc.AbcAccidental
abcAccidental Nat            = Abc.natural
abcAccidental Sharp          = Abc.sharp
abcAccidental Flat           = Abc.flat
abcAccidental DoubleSharp    = Abc.doubleSharp
abcAccidental DoubleFlat     = Abc.doubleFlat

oabcOve :: Int -> Maybe Abc.AbcOctave    
oabcOve i | i < 4       = Just $ Abc.octaveLow (4-i)
          | i > 5       = Just $ Abc.octaveHigh (i-5) 
          | otherwise   = Nothing


mkAbcDuration :: Duration -> AbcDuration
mkAbcDuration _ = dmult 1


           {-
mkAbcDuration :: Duration -> Duration -> AbcDuration
mkAbcDuration drn@(Duration _ dots) base
    case scaler base drn of
      Left i -> Just $ Abc.dmult (doti i dots)
      Right r -> Just $ factor $ dotr r dots
  where
    factor r = let (n,d) = (numerator r, denominator r)
               in if n==1 then Abc.ddiv1 d else Abc.ddiv2 (n,d)                     

-- if the duration is longer it is scaled by an int, 
-- if it is shorter its scaled by a fraction. 
scaler :: Duration -> Duration -> Either Int (Ratio Int)
scaler (Duration base _) (Duration drn _) = 
    let a = drn / base; n = numerator a; d = denominator a
    in if d == 1 then Left n else Right a
    
    
doti i dots = sum $ map (i `div`) xs
  where xs = take (dots+1) base2number_sequence

dotr r dots = sum $ map ((r /) . fromIntegral) xs
  where xs = take (dots+1) base2number_sequence  

-}  
  