
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
-- NoteLists

ppNoteList :: NoteList -> Doc
ppNoteList (NoteList se) = 
    vcat $ map (uncurry (flip ppBar)) (zip (F.toList se) [1..])

ppBar :: Int -> Bar -> Doc
ppBar i (Bar e)          = barNumber i $$ nest 4 (ppElts e)
ppBar i (Overlay se)     = 
      barNumber i $$ nest 4 (dblangles $ fsep $ punctuate 
                                               (text " // ")
                                               (map ppElts $ F.toList se))
                                               

ppElts :: Seq Elt -> Doc
ppElts = genPunctuateSeq ppElt space

ppElt :: Elt -> Doc 
ppElt (DEvt evt d)    = ppEvt evt <> durationSuffix d
ppElt (Mark m)        = ppMark m
ppElt (Chord se d)    = ppChord se <> durationSuffix d
ppElt (AGrace se p d) = ppGrace se <> char '^' <> ppPitch p <> durationSuffix d 
ppElt (UGrace p d se) = ppPitch p <> durationSuffix d <> char '^' <> ppGrace se 
              
ppEvt :: Evt Pitch -> Doc
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
              
                                                  
barNumber :: Int -> Doc
barNumber i = text "|:" <>  int i  

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
               
               
printOM :: OverlayMap -> Doc
printOM = F.foldl' fn empty . Map.toList where
    fn d (k,sse) = d  $$ text "Overlay:" <+> int k 
                      $$ vcat (map overlay (F.toList sse))
    
    overlay (i,_,se) = text "bar:" <+> int i <+> ppElts se  

                       