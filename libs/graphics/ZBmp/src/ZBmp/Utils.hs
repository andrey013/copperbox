{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZBmp.Utils
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Colour manipulation
--
--------------------------------------------------------------------------------

module ZBmp.Utils where

import ZBmp.Datatypes

data YCbCrColour = YCbCrColour { 
      _y_val  :: Float,
      _cb     :: Float,
      _cr     :: Float
    }
  deriving ( Show )    

rgbToYCbCr :: RGBcolour -> YCbCrColour
rgbToYCbCr (RGBcolour rv gv bv) = YCbCrColour y cb cr
  where
  y   =   (0.299  * r) + (0.587  * g) + (0.114  * b)
  cb  = (- 0.1687 * r) - (0.3313 * g) + (0.5    * b) + 128
  cr  =   (0.5    * r) - (0.4187 * g) - (0.0813 * b) + 128
  
  (r,g,b) = (fromIntegral rv, fromIntegral gv, fromIntegral bv)
  
  
yCbCrToRgb :: YCbCrColour -> RGBcolour
yCbCrToRgb (YCbCrColour y cb cr) = RGBcolour r g b
  where
  r = round (y + 1.402   * (cr-128.0))
  g = round (y - 0.34414 * (cb-128.0) - 0.71414 * (cr-128.0))
  b = round (y + 1.772   * (cb-128.0))


    