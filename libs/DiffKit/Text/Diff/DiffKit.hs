
--------------------------------------------------------------------------------
-- |
-- Module      :  Text.Diff.DiffKit
-- Copyright   :  (c) Stephen Tetley 2007
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  unstable
-- Portability :  portable
--
-- A library to parse and print diffs files in the unified format

--------------------------------------------------------------------------------


module Text.Diff.DiffKit
  ( module Text.Diff.DiffKit.Datatypes
  , module Text.Diff.DiffKit.Printer  
  , module Text.Diff.DiffKit.Parser  
  )
  where
  
import Text.Diff.DiffKit.Datatypes
import Text.Diff.DiffKit.Printer  
import Text.Diff.DiffKit.Parser (parseContext, parseUnified, parseNormal)
