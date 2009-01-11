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


#include <ft2build.h>
#include FT_FREETYPE_H



import Graphics.Rendering.FreeType.Internals.CBasicDataTypes

import Foreign.C.Types
import Foreign.Ptr ( Ptr, FunPtr ) 

--------------------------------------------------------------------------------

data FTraster_ 
type FTraster           = Ptr FTraster_


data FTspan = FTspan {
      _x        :: CShort,
      _len      :: CUShort,
      _coverage :: CUChar
    }


type FT_SpanFunc = CInt -> CInt -> Ptr FTspan -> VoidPtr -> IO ()


-- @FT_Raster_BitTest_Func@ is depreciated.
type FT_Raster_BitTest_Func = CInt -> CInt -> VoidPtr -> IO CInt

-- @FT_Raster_BitSet_Func@ is depreciated.
type FT_Raster_BitSet_Func = CInt -> CInt -> VoidPtr -> IO ()
                                           
                             
data FTrasterparams = FTrasterparams {
      _target           :: Ptr FTbitmap,
      _source           :: VoidPtr,
      _rparams_flags    :: FTint,
      _gray_spans       :: FunPtr FT_SpanFunc,
      _black_spans      :: FunPtr FT_SpanFunc,
      _bit_test         :: FunPtr FT_Raster_BitTest_Func,
      _bit_set          :: FunPtr FT_Raster_BitSet_Func,
      _user             :: VoidPtr,
      _clip_box         :: FTbbox
  }
  
  
-- end of file
