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
  , FretDiagramGraphic

  , fretDiagram
  , x_none

  , fretDiagAlg

  , fretNum

  , diagDef
  , diagDefsList
  
  ) where

import Neume.Core.Duration
import Neume.Core.LilyPondPretty ( spacer )
import Neume.Core.LilyPondOutput
import Neume.Core.LilyPondTrafo
import Neume.Core.Syntax

import Neume.Extra.LilyPondDoc
import Neume.Extra.LilyPondScoreOutput

import Text.PrettyPrint.Leijen                  -- package: wl-pprint

data FretNum = X | FN Int
  deriving (Eq,Show)

type FretDiagramGraphic dur = Graphic FretDiagram dur

data FretDiagram = FretDiagram 
      { chord_name  :: String
      , chord_alias :: String
      , fret_descr  :: [FretNum]
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



fretDiagAlg :: (LyRelDurTrafo repr)
            => LilyPondImageAlg repr (FretDiagramGraphic Duration)
                                     (FretDiagramGraphic (Maybe Duration))
fretDiagAlg = LilyPondImageAlg
    { glyph_printer     = renderGraphic diagOut
    , duration_trafo    = fmap runRelDurTrafo
    , pitch_trafo       = id
    }




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


