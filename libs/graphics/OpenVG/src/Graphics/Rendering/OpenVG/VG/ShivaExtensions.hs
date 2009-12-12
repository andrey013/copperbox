{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenVG.VG.ShivaExtensions
-- Copyright   :  (c) Stephen Tetley 2008, 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  unstable
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

import Graphics.Rendering.OpenVG.VG.BasicTypes ( vg_TRUE )
import Graphics.Rendering.OpenVG.VG.CFunDecls ( 
        vgCreateContextSH, vgResizeSurfaceSH, vgDestroyContextSH ) 

import Graphics.Rendering.OpenVG.VG.Utils ( unSize, unSizeM, unmarshalBool )


import Graphics.Rendering.OpenGL.GL.CoordTrans ( Size(..) )

import Control.Applicative
import Control.Monad ( ap )


-- | Create an OpenVG context, if the creation is successful run 
-- the action (destroying the context afterwards). If the 
-- creation fails run the failureAction. 
-- 
withContextSH :: Size -> (IO a) -> (IO a) -> IO a
withContextSH sz action failureAction = do
    okb <- createContextSH sz
    if okb then action >>= \ans -> destroyContextSH >> return ans
           else failureAction
 


-- | Create an OpenVG context on top of an already created 
-- OpenGL context.
--
createContextSH :: Size -> IO Bool
createContextSH = unSizeM $ \w h -> unmarshalBool <$> vgCreateContextSH w h

-- | 'resizeSurfaceSH' should be called whenever the size of the 
-- surface changes.    
--
resizeSurfaceSH :: Size -> IO ()
resizeSurfaceSH = unSizeM vgResizeSurfaceSH

-- | Destroy the OpenVG context.
destroyContextSH :: IO ()
destroyContextSH = vgDestroyContextSH

 
