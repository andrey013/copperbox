
--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Base.Printing
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Pretty print things
--
--------------------------------------------------------------------------------


module Bala.Base.Printing where

import Bala.Base.Duration
import Bala.Base.Metrical
import Bala.Base.Pitch
import Bala.Base.Structural

import HNotate.SequenceUtils (para)

import qualified Data.Foldable as F
import qualified Data.Map as Map
import Data.Sequence hiding (empty)
import Data.Ratio

import Prelude hiding (null)
import Text.PrettyPrint.HughesPJ


genPunctuateSeq :: (a -> Doc) -> Doc -> Seq a -> Doc
genPunctuateSeq pp sep = para phi empty
  where 
    phi c (se,  d)  | null se        = pp c <> d 
                    | otherwise      = pp c <> sep <> d

angles :: Doc -> Doc
angles d = text "<" <+> d <+> text ">"

dblangles :: Doc -> Doc
dblangles d = text "<<" <+> d <+> text ">>"

-- re-export @empty@ as emptyDoc 
emptyDoc :: Doc
emptyDoc = empty

--------------------------------------------------------------------------------
-- Metrical

ppClave :: Clave -> Doc
ppClave ClaveOn     = char 'x'
ppClave ClaveOff    = char '.'

ppRhythmicEvent :: RhythmicEvent -> Doc
ppRhythmicEvent (Sounds d)  = text "x'" <> ppDuration d 
ppRhythmicEvent (Rests d)   = text ".'" <> ppDuration d    
 
  
 
--------------------------------------------------------------------------------
-- Structural 
  
ppSection :: Section -> Doc
ppSection (Section tm se) = 
    vcat $ map numberedPharse (zip (F.toList se) [1..])
  where    
    numberedPharse :: (Phrase,Int) -> Doc
    numberedPharse (ph,i) = text "|:" <>  int i $$ nest 4 (ppPhrase ph) 

genPPPhraseF :: (a -> Doc) -> Doc -> PhraseF a -> Doc
genPPPhraseF f op (Single mo) = genPPMotifF f op mo
genPPPhraseF f op (Overlay mo smo)   = 
    dblangles $ fsep $ punctuate (text " // ") (map (genPPMotifF f op) xs) 
  where xs = mo : F.toList smo
  
  
ppPhrase :: Phrase -> Doc
ppPhrase (Single mo)        = ppMotif mo
ppPhrase (Overlay mo smo)   = dblangles $ fsep $ punctuate (text " // ") 
                                                           (map ppMotif xs) 
  where xs = mo : F.toList smo
                                               

genPPMotifF :: (a -> Doc) -> Doc -> MotifF a -> Doc
genPPMotifF f op (Motif mo) = genPunctuateSeq f op mo


ppMotif :: Motif -> Doc
ppMotif = genPPMotifF ppEvent space

ppEvent :: Event -> Doc 
ppEvent (Note p d)      = ppPitch p   <> durationSuffix d
ppEvent (Rest d)        = char 'r'    <> durationSuffix d
ppEvent (Chord se d)    = ppChord se  <> durationSuffix d
ppEvent (Spacer d)      = char 'z'    <> durationSuffix d
ppEvent (AGrace se p d) = ppGrace se  <> char '^' <> ppPitch p <> durationSuffix d 
ppEvent (UGrace p d se) = ppPitch p   <> durationSuffix d <> char '^' <> ppGrace se 
ppEvent (Mark m)        = ppMark m              



ppChord :: Seq Pitch -> Doc
ppChord = angles . genPunctuateSeq ppPitch space

ppGrace :: Seq (Pitch,Duration) -> Doc  
ppGrace = braces . genPunctuateSeq ppPD space
  where ppPD (p,d) = ppPitch p <> durationSuffix d


ppMark :: Mark -> Doc
ppMark Tie = char '~'
              
                                                  
 

durationSuffix :: Duration -> Doc
durationSuffix d = char '\'' <> ppDuration d       

ppPitch :: Pitch -> Doc
ppPitch (Pitch l a o) = char (toLowerLChar l) <> ppAccidental a <> int o


ppAccidental :: Accidental -> Doc
ppAccidental DoubleFlat         = text "eses"
ppAccidental Flat               = text "es"
ppAccidental Nat                = empty
ppAccidental Sharp              = text "is"
ppAccidental DoubleSharp        = text "isis"


ppDuration r = let (n,d) = (numerator r, denominator r) in
               integer n <> char '/' <> integer d    
               
               


                       