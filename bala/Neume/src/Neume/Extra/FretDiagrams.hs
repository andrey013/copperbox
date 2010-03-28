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

  , lyPhraseFretDiagrams
  , lilypondFretDiagScore
  , fretNum
  , diagDefinition
  
  ) where

import Neume.Core.BracketMarkup
import Neume.Core.Duration
import Neume.Core.LilyPondBasic
import Neume.Core.LilyPondOutput
import Neume.Core.Metrical
import Neume.Core.SyntaxMarkup
import Neume.Core.SyntaxScore
import Neume.Core.Utils

import Neume.Extra.LilyPondDoc

import Text.PrettyPrint.Leijen                  -- package: wl-pprint

data FretNum = X | FN Int
  deriving (Eq,Show)

type FretDiagramGlyph = SkipGlyph FretDiagram Duration

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

lyPhraseFretDiagrams :: MeterPattern -> [FretDiagramGlyph] -> PhraseImage
lyPhraseFretDiagrams mp xs =
    renderMarkupPhrase diagOut $ rewriteDurationOpt_
                               $ phraseMarkup mp xs

lilypondFretDiagScore :: (a -> PhraseImage) 
                   -> (BarNum -> DocS) 
                   -> (() -> LyChordSc a) 
                   -> ScoreImage
lilypondFretDiagScore _ _ _ = undefined

newtype LyChordSc a = LyChordSc ()

diagOut :: FretDiagram -> Maybe Duration -> Doc
diagOut (FretDiagram _ alias _) od = spacer od `annoAbove` variableUse alias


diagDefinition :: FretDiagram -> Doc
diagDefinition (FretDiagram name alias xs) = 
  comment name <$> variableDef alias (markup $ diagram xs)

diagram :: [FretNum] -> Doc
diagram xs = command "fret-diagram" <+> (schemeStringLiteral diag_text)
  where
    diag_text = show $ hcat $ punctuate semi (pre : body)
    pre       = char 'w' <> int (length xs)
    body      = fst $ foldr fn ([],1) xs

    fn X      (acc,i) = (int i <> text "-x" : acc,         i+1)
    fn (FN 0) (acc,i) = (int i <> text "-o" : acc,         i+1)
    fn (FN n) (acc,i) = (int i <> char '-' <> int n : acc, i+1)


