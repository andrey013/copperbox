{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.FreeType.Outline
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- ...
--
--------------------------------------------------------------------------------

module Graphics.Rendering.FreeType.Outline (
  
  FT_outline, -- expose the alias not the type (so it is opaque).
  withOutline,
  contours,
  
) where

import Graphics.Rendering.FreeType.Internals.CBaseTypes ( 
      FT_library(..)  )
-- import Graphics.Rendering.FreeType.Internals.CBasicDataTypes
import Graphics.Rendering.FreeType.Internals.COutline
-- import Graphics.Rendering.FreeType.Internals.Wrappers 




import Foreign.ForeignPtr ( withForeignPtr )
import Foreign.Marshal.Utils ( with )
import Foreign.Ptr ( nullPtr )
import Foreign.Storable ( peek ) 




-------------------------------------------------------------------------------- 


-- FT_outline is not an opaque handle.
-- We need to be able to create one and pass it to the C-side.

mkOutline :: FT_outline
mkOutline = FT_struct_outline {
      _n_contours     = 0,
      _n_points       = 0,
      _points         = nullPtr,
      _tags           = nullPtr,
      _contours       = nullPtr,
      _outline_flags  = 0
    }
       

withOutline :: FT_library -> Int -> Int -> a -> (FT_outline -> IO a) -> IO a
withOutline (FT_library lib) max_pts max_cts failureValue action = 
  withForeignPtr lib $ \h -> 
    with mkOutline $ \ptval -> do 
      ec <- ft_outline_new h (fromIntegral max_pts) (fromIntegral max_cts) ptval
      if (ec == 0) 
        then do 
          ans <- action =<< peek ptval
          ft_outline_done h ptval
          return ans
        else
          return failureValue
      

                      

-- the _with_ pattern is from Data.Time.Clock.CTimeval from the time package 
  
  


contours :: FT_outline -> IO Int
contours = return . fromIntegral . _n_contours

          
          
