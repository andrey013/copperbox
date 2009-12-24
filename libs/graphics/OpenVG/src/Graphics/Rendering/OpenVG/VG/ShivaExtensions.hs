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
  createContextSH,
  resizeSurfaceSH, 
  destroyContextSH,

  withContextSH

) where

import Graphics.Rendering.OpenVG.VG.Utils ( unmarshalBool, unSizeM )

import Graphics.Rendering.OpenVG.Raw.VG.ShivaExtensions

import Graphics.Rendering.OpenGL.GL.CoordTrans ( Size(..) )

import Control.Applicative



-- | Create an OpenVG context on top of a previously created 
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
--
destroyContextSH :: IO ()
destroyContextSH = vgDestroyContextSH

 

-- | Create an OpenVG context, if the creation is successful run 
-- the action and destroy the context afterwards. If the 
-- creation fails run the failureAction. 
-- 
withContextSH :: Size -> (IO a) -> (IO a) -> IO a
withContextSH sz action failureAction = do
    okb <- createContextSH sz
    if okb then action >>= \ans -> destroyContextSH >> return ans
           else failureAction
 
