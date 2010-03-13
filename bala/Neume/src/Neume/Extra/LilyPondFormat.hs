{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Neume.Extra.LilyPondFormat
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


module Neume.Extra.LilyPondFormat
  (
  
    simpleOutput

  , parallelPhrases

  ) where

import Neume.Core.Duration
import Neume.Core.LilyPondBasic ( spacer )
import Neume.Core.SyntaxDoc

import Neume.Extra.LilyPondDoc

import Text.PrettyPrint.Leijen                  -- package: wl-pprint

import Data.List ( foldl' )

simpleOutput :: Phrase LY -> Doc
simpleOutput = vsep . map (<+> singleBar) . getPhrase





-- Handily parallel overlays are 'context free' 

-- More convoluted than ABC as LilyPond \\parallelMusic has
-- to be a proper matrix.
--

parallelPhrases :: Duration -> [Phrase LY] -> [OverlayBar]
parallelPhrases _       []     = []
parallelPhrases bar_len (x:xs) = snd $ foldl' (parallel2 bar_len) (1,parallel1 x) xs

type Depth = Int

parallel2  :: Duration -> (Int,[OverlayBar]) -> Phrase LY -> (Int,[OverlayBar])
parallel2 bar_len (depth,bs1) (Phrase bs2) = (depth+1, step bs1 bs2) where
  step (x:xs) (y:ys) = parallelLy x y : step xs ys
  step (x:xs) []     = parallelLy x (spacer $ Just bar_len) : step xs [] 
  step []     (y:ys) = parallelLy (blank depth bar_len) y : step [] ys 
  step []     []     = []

blank :: Depth -> Duration -> OverlayBar
blank n d = step (OverlayBar sbar) (n-1) where
  step bar i | i <= 0    = bar
             | otherwise = step (parallelLy bar sbar) (i-1)

  sbar       = spacer $ Just d

parallel1 :: Phrase LY -> [OverlayBar]
parallel1 = map OverlayBar . getPhrase

parallelLy :: OverlayBar -> Doc -> OverlayBar
parallelLy (OverlayBar v1) v2 = OverlayBar $ v1 <+> singleBar <$> v2 




-- Are you going to need an amalgamate monad?
-- if so it can e.g put "||" at the end of a passage if the end 
-- isn\'t a repeat.

-- repeated :: Phrase -> [Bar] -> Doc

-- Check \parallelMusic ...  page 118 (pdf page 128)
-- notes are relative to the previous note in the voice - not 
-- the previous note in the input

-- accounting for overlays / parallel music takes the list-of Bars
-- to a forest of one-or-many Bars - [(OneMany Bar)]