{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenVG.VG.ColorTransformation
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- This module corresponds to section 13 (Color Transformation and Blending) 
-- of the OpenVG 1.1 specs.
--
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenVG.VG.ColorTransformation where

import Graphics.Rendering.OpenVG.VG.BasicTypes ( VGenum )
import Graphics.Rendering.OpenVG.Constants (
    vg_BLEND_SRC, vg_BLEND_SRC_OVER, vg_BLEND_DST_OVER, 
    vg_BLEND_SRC_IN, vg_BLEND_DST_IN, vg_BLEND_MULTIPLY, 
    vg_BLEND_SCREEN, vg_BLEND_DARKEN, vg_BLEND_LIGHTEN, 
    vg_BLEND_ADDITIVE ) 
    
data BlendMode = 
     BlendSrc
   | BlendSrcOver
   | BlendDstOver
   | BlendSrcIn
   | BlendDstIn
   | BlendMultiply
   | BlendScreen
   | BlendDarken
   | BlendLighten
   | BlendAdditive
   deriving ( Eq, Ord, Show )
   
marshalBlendMode :: BlendMode -> VGenum
marshalBlendMode x = case x of
    BlendSrc -> vg_BLEND_SRC
    BlendSrcOver -> vg_BLEND_SRC_OVER
    BlendDstOver -> vg_BLEND_DST_OVER
    BlendSrcIn -> vg_BLEND_SRC_IN
    BlendDstIn -> vg_BLEND_DST_IN
    BlendMultiply -> vg_BLEND_MULTIPLY
    BlendScreen -> vg_BLEND_SCREEN
    BlendDarken -> vg_BLEND_DARKEN
    BlendLighten -> vg_BLEND_LIGHTEN
    BlendAdditive -> vg_BLEND_ADDITIVE
    

