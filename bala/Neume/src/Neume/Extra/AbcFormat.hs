{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Neume.Extra.AbcFormat
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Formatting operations for bars (repeats etc.)
--
--------------------------------------------------------------------------------


module Neume.Extra.AbcFormat
  (

    simpleOutput
  , tempOutput

  , overlayPhrases

  ) where

import Neume.Core.SyntaxDoc
import Neume.Extra.AbcDoc

import Text.PrettyPrint.Leijen          -- package: wl-pprint

import Data.List ( foldl' )

-- | Output ABC, four bars printed on each line. 
simpleOutput :: AbcPhrase -> Doc
simpleOutput = four . map ((<+> singleBar) . getAbcBar) . getAbcPhrase


tempOutput :: [OverlayBar] -> Doc
tempOutput = four . map ((<+> singleBar) . getOverlayBar) 


four :: [Doc] -> Doc
four (a:b:c:d:xs) = vsep (map (<> lineCont) [a,b,c]) <$> d <$> four xs
four xs           = hsep xs





-- Handily overlays are 'context free' 

overlayPhrases :: [AbcPhrase] -> [OverlayBar]
overlayPhrases []     = []
overlayPhrases (x:xs) = foldl' overlay2 (overlay1 x) xs


overlay2  :: [OverlayBar] -> AbcPhrase -> [OverlayBar]
overlay2 bs1 (AbcPhrase bs2) = step bs1 bs2 where
  step (x:xs) (y:ys) = overlayAbc x (getAbcBar y) : step xs ys
  step xs     []     = xs 
  step []     ys     = map (OverlayBar . getAbcBar) ys 

overlay1 :: AbcPhrase -> [OverlayBar]
overlay1 = map (OverlayBar . getAbcBar) . getAbcPhrase

overlayAbc :: OverlayBar -> Doc -> OverlayBar
overlayAbc (OverlayBar v1) v2 = OverlayBar $ v1 <+> overlay <> lineCont <$> v2 
