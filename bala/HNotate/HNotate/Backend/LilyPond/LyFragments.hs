
--------------------------------------------------------------------------------
-- |
-- Module      :  HNotate.Backend.LilyPond.LyFragments
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Datatypes of the fragments produced during rendering. 
--
--------------------------------------------------------------------------------

module HNotate.Backend.LilyPond.LyFragments (
    PartLyExprs, PartLyMusicExpr
  ) where

import HNotate.Print.OutputLilyPond
import Data.Sequence


type PartLyExprs = (Seq PartLyMusicExpr)
type PartLyMusicExpr = LyCxt_Element


