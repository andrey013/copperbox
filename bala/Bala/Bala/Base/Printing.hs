
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
import Bala.Base.OutputMidi
import Bala.Base.Pitch
import Bala.Base.Structural

import HNotate.CommonUtils (para)

import qualified Data.Foldable as F
import qualified Data.Map as Map
import Data.Sequence hiding (empty)
import Data.Ratio

import Prelude hiding (null)
import Text.PrettyPrint.HughesPJ



genPunctuateSeq :: (a -> Doc) -> Doc -> Seq a -> Doc
genPunctuateSeq pp sep = para phi empty
  where 
    phi c (se,  d)  | null se        = pp c <+> d 
                    | otherwise      = pp c <> sep <+> d

angles :: Doc -> Doc
angles d = text "<" <+> d <+> text ">"

dblangles :: Doc -> Doc
dblangles d = text "<<" <+> d <+> text ">>"
 
--------------------------------------------------------------------------------
-- Structural 
  
ppSection :: Section -> Doc
ppSection (Section tm se) = 
    vcat $ map numberedPharse (zip (F.toList se) [1..])
  where    
    numberedPharse :: (Phrase,Int) -> Doc
    numberedPharse (ph,i) = text "|:" <>  int i $$ nest 4 (ppPhrase ph) 



ppPhrase :: Phrase -> Doc
ppPhrase (Single mo)        = ppMotif mo
ppPhrase (Overlay mo smo)   = dblangles $ fsep $ punctuate (text " // ") 
                                                           (map ppMotif xs) 
  where xs = mo : F.toList smo
                                               



ppMotif :: Motif -> Doc
ppMotif (Motif mo) = genPunctuateSeq ppElt space mo

ppElt :: Elt -> Doc 
ppElt (DEvt evt d)    = ppEvt evt <> durationSuffix d
ppElt (Mark m)        = ppMark m
ppElt (Chord se d)    = ppChord se <> durationSuffix d
ppElt (AGrace se p d) = ppGrace se <> char '^' <> ppPitch p <> durationSuffix d 
ppElt (UGrace p d se) = ppPitch p <> durationSuffix d <> char '^' <> ppGrace se 
              
ppEvt :: Evt -> Doc
ppEvt (Note p)  = ppPitch p
ppEvt Rest      = char 'r'
ppEvt Spacer    = char 'z'

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
               
               


                       