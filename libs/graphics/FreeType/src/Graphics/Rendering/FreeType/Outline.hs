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
  copy,
  translate,
  transform,
  contours,
  points,
  embolden,
  check,
  getBBox,
  
  getOrientation,
  
) where

import Graphics.Rendering.FreeType.Internals.CBaseTypes ( 
      FT_library(..)  )
import Graphics.Rendering.FreeType.Internals.CBasicDataTypes ( 
      FT_pos, FT_error, Matrix, BBox(..) )
import Graphics.Rendering.FreeType.Internals.COutline
import Graphics.Rendering.FreeType.Utils ( Unmarshal(..) )




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
      
-- Outline API functions

copy :: FT_outline -> FT_outline -> IO FT_error 
copy src dest = 
    with src $ \sptr ->
        with dest $ \dptr -> ft_outline_copy sptr dptr 


translate :: FT_outline -> FT_pos -> FT_pos -> IO ()
translate outl x y = with outl $ \ptr -> ft_outline_translate ptr x y


transform :: FT_outline -> Matrix -> IO ()
transform outl mat = 
    with outl $ \optr -> 
        with mat $ \mptr -> ft_outline_transform optr mptr

embolden :: FT_outline -> FT_pos -> IO FT_error
embolden outl s = with outl $ \ptr -> ft_outline_embolden ptr s

{-
-- From the FreeType docs
-- [FT_Outline_Reverse] shouldn't be used by a normal client application, 
-- unless it knows what it is doing.

oreverse :: FT_outline -> IO ()
oreverse outl = with outl $ \ptr -> ft_outline_reverse ptr 
-}

check :: FT_outline -> IO FT_error
check outl = with outl $ \ptr -> ft_outline_check ptr 

getBBox :: FT_outline -> IO (Maybe BBox)
getBBox outl = 
    with (BBox 0 0 0 0) $ \bbox -> 
        with outl $ \ptr -> do 
            ec <- ft_outline_get_bbox ptr bbox
            if (ec == 0) 
               then peek bbox >>= return . Just
               else return Nothing

getOrientation :: FT_outline -> IO Orientation
getOrientation outl = 
    with outl $ \ptr -> ft_outline_get_orientation ptr >>= return . unmarshal

                                            
contours :: FT_outline -> IO Int
contours = return . fromIntegral . _n_contours

points :: FT_outline -> IO Int
points = return . fromIntegral . _n_points     
          
