
--------------------------------------------------------------------------------
-- |
-- Module      :  Text.Diff.UnifiedDiff
-- Copyright   :  (c) Stephen Tetley 2007
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- A library to parse and print diffs files in the unified format

--------------------------------------------------------------------------------


module Text.Diff.UnifiedDiff 
  ( module Text.Diff.Unified.Datatypes
  , module Text.Diff.Unified.Printer  
  , module Text.Diff.Unified.Parser  
  )
  where
  
import Text.Diff.Unified.Datatypes
import Text.Diff.Unified.Printer  
import Text.Diff.Unified.Parser
