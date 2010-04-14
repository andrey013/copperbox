{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Neume.Extra.FretDiagrams
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Fret diagrams for LilyPond.
--
--------------------------------------------------------------------------------

module Neume.Extra.FretDiagrams
  (
    FretNum(..)
  , FretDiagram(..)
  , FretDiagramGlyph

  , fretDiagram
  , x_none

  , Ly_Fret_Diag_Config(..)
  , renderFretDiag
  , fretNum

  , diagDef
  , diagDefsList
  
  ) where

import Neume.Core.Bracket
import Neume.Core.Duration
import Neume.Core.LilyPondBasic
import Neume.Core.LilyPondOutput
import Neume.Core.Metrical
import Neume.Core.SyntaxGlyph
import Neume.Core.SyntaxInterim
import Neume.Core.SyntaxNoteList
import Neume.Core.SyntaxScore

import Neume.Extra.LilyPondDoc
import Neume.Extra.LilyPondFormat

import Text.PrettyPrint.Leijen                  -- package: wl-pprint

data FretNum = X | FN Int
  deriving (Eq,Show)

type FretDiagramGlyph = MarkupGlyph FretDiagram Duration

data FretDiagram = FretDiagram { 
        chord_name  :: String,
        chord_alias :: String,
        fret_descr  :: [FretNum]
      }
  deriving (Show)



-- | negative numbers are interpreted as 'X'.
--
fretNum :: Int -> FretNum
fretNum n | n < 0     = X
          | otherwise = FN n

x_none :: Int
x_none = -1

fretDiagram :: String -> String -> [Int] -> FretDiagram
fretDiagram name alias ns = FretDiagram name alias (map fretNum ns)


data Ly_Fret_Diag_Config = Ly_Fret_Diag_Config
    { meter_pattern_fret_diag         :: MeterPattern }

renderFretDiag :: Ly_Std_Format_Config
               -> Ly_Fret_Diag_Config
               -> Score sh (NoteList FretDiagramGlyph)
               -> Doc
renderFretDiag (Ly_Std_Format_Config func) rw1 = 
    concatDocSections func . scoreImageFretDiag rw1


scoreImageFretDiag :: Ly_Fret_Diag_Config
                   -> Score sh (NoteList FretDiagramGlyph) 
                   -> Score sh PhraseImage
scoreImageFretDiag cfg = fmap (phraseImageFretDiag mp)
  where
    mp = meter_pattern_fret_diag cfg




phraseImageFretDiag :: MeterPattern
                    -> NoteList FretDiagramGlyph
                    -> PhraseImage
phraseImageFretDiag mp = 
  renderPhrase drawDiagGlyph . rewriteDurationOpt . phrase mp



drawDiagGlyph :: MarkupGlyph FretDiagram (Maybe Duration) -> Doc
drawDiagGlyph (MGlyph fd od) = diagOut fd od
drawDiagGlyph (Skip od)      = spacer od

diagOut :: FretDiagram -> Maybe Duration -> Doc
diagOut (FretDiagram _ alias _) od = spacer od `annoAbove` variableUse alias


diagDef :: FretDiagram -> Doc
diagDef (FretDiagram name alias xs) = 
  lineComment name <$> variableDef alias (markup $ diagram xs)

diagDefsList :: [FretDiagram] -> Doc
diagDefsList xs = lineComment "Fret diagrams ..."
               <$> vcat (map diagDef xs) 


diagram :: [FretNum] -> Doc
diagram xs = command "fret-diagram" <+> (schemeStringLiteral diag_text)
  where
    diag_text = show $ pre <> body
    pre       = char 'w' <> colon <> int (length xs) <> semi
    body      = fst $ foldr fn (empty,1) xs

    fn X      (acc,i) = (int i <> text "-x;" <> acc,                  i+1)
    fn (FN 0) (acc,i) = (int i <> text "-o;" <> acc,                  i+1)
    fn (FN n) (acc,i) = (int i <> char '-'   <> int n <> semi <> acc, i+1)


