{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenVG.VG.Blending
-- Copyright   :  (c) Stephen Tetley 2008, 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  unstable
-- Portability :  GHC
--
-- This module corresponds to section 12 (Blending) 
-- of the OpenVG 1.0.1 specs.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenVG.VG.Blending (
  -- * Setting the blend mode
  BlendMode(..), 
  blendMode
) where

import Graphics.Rendering.OpenVG.VG.BasicTypes ( VGenum )
import Graphics.Rendering.OpenVG.VG.Constants (
    vg_BLEND_SRC, vg_BLEND_SRC_OVER, vg_BLEND_DST_OVER, 
    vg_BLEND_SRC_IN, vg_BLEND_DST_IN, vg_BLEND_MULTIPLY, 
    vg_BLEND_SCREEN, vg_BLEND_DARKEN, vg_BLEND_LIGHTEN, 
    vg_BLEND_ADDITIVE ) 
import Graphics.Rendering.OpenVG.VG.Parameters ( 
    seti, ParamType ( BlendMode ) )

import Data.StateVar (
   SettableStateVar, makeSettableStateVar ) 


--------------------------------------------------------------------------------
-- Setting the blend mode
   
-- | 'BlendMode' enumerates the possible blend modes.        
data BlendMode = 
     Src
   | SrcOver
   | DstOver
   | SrcIn
   | DstIn
   | Multiply'      -- Unfortunate name conflict
   | Screen
   | Darken
   | Lighten
   | Additive
   deriving ( Eq, Ord, Show )


-- | Set the blend mode.
--
-- 'blendMode' is a write-only state variable corresponding to
-- @VG_BLEND_MODE@:
--    
-- > vgSeti(VG_BLEND_MODE, mode);
--
blendMode :: SettableStateVar BlendMode
blendMode = makeSettableStateVar $ 
    seti BlendMode . fromIntegral . marshalBlendMode

--------------------------------------------------------------------------------

marshalBlendMode :: BlendMode -> VGenum
marshalBlendMode x = case x of
    Src       -> vg_BLEND_SRC
    SrcOver   -> vg_BLEND_SRC_OVER
    DstOver   -> vg_BLEND_DST_OVER
    SrcIn     -> vg_BLEND_SRC_IN
    DstIn     -> vg_BLEND_DST_IN
    Multiply' -> vg_BLEND_MULTIPLY
    Screen    -> vg_BLEND_SCREEN
    Darken    -> vg_BLEND_DARKEN
    Lighten   -> vg_BLEND_LIGHTEN
    Additive  -> vg_BLEND_ADDITIVE
    


