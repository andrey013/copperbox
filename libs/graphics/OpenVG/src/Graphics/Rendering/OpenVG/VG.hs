{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenVG.VG
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- A Haskell binding for the OpenVG vector and raster graphics API 
-- (version 1.0.1).
-- The implementation targets the Shiva-VG implementation which is not 
-- complete. Image Filters and Querying Hardware are not implemented. 
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenVG.VG (
  module Graphics.Rendering.OpenVG.VG.BasicTypes,  
  module Graphics.Rendering.OpenVG.VG.Blending,
  module Graphics.Rendering.OpenVG.VG.DrawingContext,
  module Graphics.Rendering.OpenVG.VG.Extending,
  module Graphics.Rendering.OpenVG.VG.Images,
  module Graphics.Rendering.OpenVG.VG.Paint,
  module Graphics.Rendering.OpenVG.VG.Parameters,
  module Graphics.Rendering.OpenVG.VG.Paths,
  module Graphics.Rendering.OpenVG.VG.RenderingQuality,
  module Graphics.Rendering.OpenVG.VG.Scissoring,
  module Graphics.Rendering.OpenVG.VG.ShivaExtensions
) where

import Graphics.Rendering.OpenVG.VG.BasicTypes
import Graphics.Rendering.OpenVG.VG.Blending
import Graphics.Rendering.OpenVG.VG.DrawingContext
import Graphics.Rendering.OpenVG.VG.Extending
import Graphics.Rendering.OpenVG.VG.Images
import Graphics.Rendering.OpenVG.VG.Paint
import Graphics.Rendering.OpenVG.VG.Parameters
import Graphics.Rendering.OpenVG.VG.Paths
import Graphics.Rendering.OpenVG.VG.RenderingQuality
import Graphics.Rendering.OpenVG.VG.Scissoring
import Graphics.Rendering.OpenVG.VG.ShivaExtensions
