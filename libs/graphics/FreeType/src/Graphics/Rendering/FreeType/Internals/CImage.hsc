{-# LANGUAGE CPP                        #-}
{-# LANGUAGE ForeignFunctionInterface   #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.FreeType.Internals.CImage
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Internal module corresponding to FT_IMAGE_H.
--
--
--------------------------------------------------------------------------------

module Graphics.Rendering.FreeType.Internals.CImage where






import Graphics.Rendering.FreeType.Internals.CBasicDataTypes

import Foreign.C.Types
import Foreign.Ptr ( Ptr, FunPtr ) 

--------------------------------------------------------------------------------

data FT_RASTER_RCRD_ 
type FT_raster_ptr        = Ptr FT_RASTER_RCRD_


data FT_struct_span = FT_struct_span {
      _x        :: CShort,
      _len      :: CUShort,
      _coverage :: CUChar
    }


type FT_spanfunc = CInt -> CInt -> Ptr FT_struct_span -> VoidPtr -> IO ()


-- @FT_Raster_BitTest_Func@ is depreciated.
type FT_raster_bittest_func = CInt -> CInt -> VoidPtr -> IO CInt

-- @FT_Raster_BitSet_Func@ is depreciated.
type FT_raster_bitset_func = CInt -> CInt -> VoidPtr -> IO ()
                                           
                             
data FT_struct_rasterparams = FT_struct_rasterparams {
      _target           :: Ptr FT_struct_bitmap,
      _source           :: VoidPtr,
      _rparams_flags    :: FT_int,
      _gray_spans       :: FunPtr FT_spanfunc,
      _black_spans      :: FunPtr FT_spanfunc,
      _bit_test         :: FunPtr FT_raster_bittest_func,
      _bit_set          :: FunPtr FT_raster_bitset_func,
      _user             :: VoidPtr,
      _clip_box         :: FT_struct_bbox
  }
  
  
-- end of file
