
--------------------------------------------------------------------------------
-- |
-- Module      :  HNotate.Backend.Abc.AbcFragments
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

module HNotate.Backend.Abc.AbcFragments (
    PartAbcExprs, PartAbcMusicExpr
  ) where

import HNotate.Print.OutputAbc
import Data.Sequence


type PartAbcExprs = (Seq PartAbcMusicExpr)
type PartAbcMusicExpr = AbcCxt_Body


