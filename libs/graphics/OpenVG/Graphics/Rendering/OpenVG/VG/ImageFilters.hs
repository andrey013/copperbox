{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenVG.VG.ImageFilters
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- This module corresponds to section 12 (Image Filters) 
-- of the OpenVG 1.1 specs.
--
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenVG.VG.ImageFilters where

import Graphics.Rendering.OpenVG.VG.BasicTypes ( VGenum )
import Graphics.Rendering.OpenVG.VG.Constants ( 
    vg_RED, vg_GREEN, vg_BLUE, vg_ALPHA
  )
  
data ImageChannel =
     ImageChannelRed
   | ImageChannelGreen
   | ImageChannelBlue
   | ImageChannelAlpha
   deriving ( Eq, Ord, Show )
   
marshalImageChannel :: ImageChannel -> VGenum
marshalImageChannel x = case x of 
    ImageChannelRed -> vg_RED
    ImageChannelGreen -> vg_GREEN
    ImageChannelBlue -> vg_BLUE
    ImageChannelAlpha -> vg_ALPHA
    
    

