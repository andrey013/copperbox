{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.OTFont.Glyph.Datatypes
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Glyph datatypes
--
--------------------------------------------------------------------------------

module Graphics.OTFont.Glyph.Datatypes where

import Graphics.OTFont.Datatypes

data OutlinePoint = OnCurvePt Short Short
                  | OffCurvePt Short Short
  deriving (Eq,Ord,Show)
  
  

    