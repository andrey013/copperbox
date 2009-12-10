{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenVG.VG.ShivaExtensions
-- Copyright   :  (c) Stephen Tetley 2008, 2009
-- License     :  BSD3
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
  
  withContextSH, 
  
  createContextSH,
  resizeSurfaceSH, 
  destroyContextSH
) where

import Graphics.Rendering.OpenVG.VG.BasicTypes ( vg_TRUE, unSize )
import Graphics.Rendering.OpenVG.VG.CFunDecls ( 
        vgCreateContextSH, vgResizeSurfaceSH, vgDestroyContextSH ) 

import Graphics.Rendering.OpenGL.GL.CoordTrans ( Size(..) )

-- | Create an OpenVG context, if the creation is successful run the 
-- action (destroying the context afterwards). If the creation fails
-- run the failureAction. 
withContextSH :: Size -> (IO a) -> (IO a) -> IO a
withContextSH sz action failureAction = do
    okb <- createContextSH sz
    if okb then action >>= \ans -> destroyContextSH >> return ans
           else failureAction
 


-- | Create an OpenVG context on top of an already created OpenGL context.   
createContextSH :: Size -> IO Bool
createContextSH sz = let (w,h) = unSize sz in do 
    vgbool <- vgCreateContextSH w h
    if vgbool == vg_TRUE then return True else return False

-- | @resizeSurfaceSH@ should be called whenever the size of the 
-- surface changes.    
resizeSurfaceSH :: Size -> IO ()
resizeSurfaceSH sz = let (w,h) = unSize sz in vgResizeSurfaceSH w h

-- | Destroy the OpenVG context.
destroyContextSH :: IO ()
destroyContextSH = vgDestroyContextSH

 
