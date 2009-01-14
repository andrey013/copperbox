{-# LANGUAGE CPP                        #-}
{-# LANGUAGE ForeignFunctionInterface   #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.FreeType.Internals.CFreeTypeVersion
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Internal module for accessing the FreeType Version number
--
--
--------------------------------------------------------------------------------

module Graphics.Rendering.FreeType.Internals.CFreeTypeVersion where

#include <ft2build.h>
#include FT_FREETYPE_H

import Graphics.Rendering.FreeType.Internals.CBasicDataTypes

import Foreign.Ptr ( Ptr )

-- | @FT_Library_Version@.
foreign import ccall unsafe "freetype/freetype.h FT_Library_Version" 
    ft_library_version  :: FT_library_ptr 
                        -> Ptr FT_int 
                        -> Ptr FT_int 
                        -> Ptr FT_int 
                        -> IO ()
                        
-- end of file
