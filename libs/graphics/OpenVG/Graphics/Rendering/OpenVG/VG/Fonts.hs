{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenVG.VG.Fonts
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- This module corresponds to section 11 (Fonts) 
-- of the OpenVG 1.1 specs.
--
--
--------------------------------------------------------------------------------
{- Not implemented by shiva-vg -}
module Graphics.Rendering.OpenVG.VG.Fonts where

import Graphics.Rendering.OpenVG.VG.BasicTypes ( VGenum )
import Graphics.Rendering.OpenVG.VG.Constants ( vg_FONT_NUM_GLYPHS )

data FontParamType = 
     FontNumGlyphs
     deriving ( Eq, Ord, Show )
     
marshalFontParamType :: FontParamType -> VGenum
marshalFontParamType x = case x of
    FontNumGlyphs -> vg_FONT_NUM_GLYPHS
    