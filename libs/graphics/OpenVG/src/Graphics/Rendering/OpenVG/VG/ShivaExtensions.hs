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
  -- * ShivaVG extensions
  createContextSH,
  resizeSurfaceSH, 
  destroyContextSH
) where

import Graphics.Rendering.OpenVG.VG.BasicTypes ( vg_TRUE )
import Graphics.Rendering.OpenVG.VG.CFunDecls ( 
        vgCreateContextSH, vgResizeSurfaceSH, vgDestroyContextSH ) 

import Graphics.Rendering.OpenGL.GL.CoordTrans ( Size(..) )


-- | Create an OpenVG context on top of an already created OpenGL context.   
createContextSH :: Size -> IO Bool
createContextSH (Size w h) = do 
    vgbool <- vgCreateContextSH w h
    if vgbool == vg_TRUE then return True else return False

-- | @resizeSurfaceSH@ should be called whenever the size of the 
-- surface changes.    
resizeSurfaceSH :: Size -> IO ()
resizeSurfaceSH (Size w h) = vgResizeSurfaceSH w h

-- | Destroy the OpenVG context.
destroyContextSH :: IO ()
destroyContextSH = vgDestroyContextSH

 
