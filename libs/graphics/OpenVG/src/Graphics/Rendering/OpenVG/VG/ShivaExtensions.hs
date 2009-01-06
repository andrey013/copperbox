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

import Graphics.Rendering.OpenVG.VG.BasicTypes ( vg_TRUE )
import Graphics.Rendering.OpenVG.VG.CFunDecls ( 
        vgCreateContextSH, vgResizeSurfaceSH, vgDestroyContextSH ) 

import Graphics.Rendering.OpenGL.GL.CoordTrans ( Size(..) )

-- Note - I\'m not sure if this is the best type for the function...
-- HOpenGL uses GLints for window positions, sizes etc, so it makes sense to 
-- follow that convention, and it seems preferable to return a Haskell Bool
-- but this means the function has mixed high-level and low-level types.
   
createContextSH :: Size -> IO Bool
createContextSH (Size w h) = do 
    vgbool <- vgCreateContextSH w h
    if vgbool == vg_TRUE then return True else return False
    
resizeSurfaceSH :: Size -> IO ()
resizeSurfaceSH (Size w h) = vgResizeSurfaceSH w h

destroyContextSH :: IO ()
destroyContextSH = vgDestroyContextSH

 
