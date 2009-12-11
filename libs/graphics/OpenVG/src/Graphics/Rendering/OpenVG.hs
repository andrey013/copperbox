{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenVG
-- Copyright   :  (c) Stephen Tetley 2008, 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  unstable
-- Portability :  GHC
--
-- Import (convenience) module for the VG and VGU library modules.
-- 
-- A Haskell binding for the OpenVG vector and raster graphics API.
--
-- The implementation targets the Shiva-VG implementation which 
-- is not complete. Image Filters and Querying Hardware are not 
-- implemented and various functions are missing. 

--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenVG (
  module Graphics.Rendering.OpenVG.VG,
  module Graphics.Rendering.OpenVG.VGU,
) where

import Graphics.Rendering.OpenVG.VG
import Graphics.Rendering.OpenVG.VGU
