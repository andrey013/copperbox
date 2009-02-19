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

import Data.Sequence


data Glyph = 
      SimpleGlyph { 
          glyph_name      :: String, 
          glyph_contours  :: Seq Contour
        }
    | CompositeGlyph {
          glyph_name      :: String
          -- ... TODO
        }
  deriving (Eq,Show)    

             
newtype Contour = Contour { getContour :: Seq OutlinePoint }
  deriving (Eq,Show)

data OutlinePoint = OnCurvePt Short Short
                  | OffCurvePt Short Short
  deriving (Eq,Ord,Show)
  
  

    