{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenVG.VG.Utils
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Utility functions.
--
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenVG.VG.Utils (
  Marshal(..), bitwiseOr
  
) where


import Graphics.Rendering.OpenVG.VG.BasicTypes ( VGenum, VGbitfield )

class Marshal a where marshal :: a -> VGenum

bitwiseOr :: Marshal a => [a] -> VGbitfield
bitwiseOr = sum . map (fromIntegral . marshal)


