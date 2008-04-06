--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Format.LilyPond.Pretty
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Pretty printer for a subset of LilyPond files
--
--------------------------------------------------------------------------------

module Bala.Format.LilyPond.Pretty where

import Bala.Format.LilyPond.Datatypes

import Text.PrettyPrint.Leijen

ppStringMark :: String -> Doc
ppStringMark ss = char '^' <> dquotes (text ss)


