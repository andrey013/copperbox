{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenVG.VG.ShivaExtensions
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Shiva extensions to replace EGL - see the README in the shiva-vg
-- archive.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenVG.VG.ShivaExtensions (
  createContextSH,
  resizeSurfaceSH, 
  destroyContextSH
) where

import Graphics.Rendering.OpenVG.VG.BasicTypes ( 
        VGint, vg_TRUE )
import Graphics.Rendering.OpenVG.VG.CFunDecls ( 
        vgCreateContextSH, vgResizeSurfaceSH, vgDestroyContextSH ) 

-- Note - I\'m not sure if this is the best type for the function...
-- HOpenGL uses GLints for window positions, sizes etc, so it makes sense to 
-- follow that convention, and it seems preferable to return a Haskell Bool
-- but this means the function has mixed high-level and low-level types.
   
createContextSH :: VGint -> VGint -> IO Bool
createContextSH width height = do 
    vgbool <- vgCreateContextSH width height
    if vgbool == vg_TRUE then return True else return False
    
resizeSurfaceSH :: VGint -> VGint -> IO ()
resizeSurfaceSH width height = vgResizeSurfaceSH width height

destroyContextSH :: IO ()
destroyContextSH = vgDestroyContextSH

 
