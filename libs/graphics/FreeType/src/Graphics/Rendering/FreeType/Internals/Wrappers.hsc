{-# LANGUAGE CPP                        #-}
{-# LANGUAGE ForeignFunctionInterface   #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.FreeType.Internals.Wrappers
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

module Graphics.Rendering.FreeType.Internals.Wrappers where

import Graphics.Rendering.FreeType.Internals.CBaseTypes
-- import Graphics.Rendering.FreeType.Internals.CBasicDataTypes
-- import Graphics.Rendering.FreeType.Internals.COutline

import Foreign.Ptr ( FunPtr ) 

--------------------------------------------------------------------------------

foreign import ccall "wrapper"
    mkDoneLibrary       :: (FT_library_ptr -> IO ()) 
                        -> IO (FunPtr (FT_library_ptr -> IO ()))
  
foreign import ccall "wrapper"
    mkDoneFace          :: (FT_face_ptr -> IO ()) 
                        -> IO (FunPtr (FT_face_ptr -> IO ()))

foreign import ccall "wrapper"
    mkDoneOutline       :: (FT_outline_ptr -> IO ()) 
                        -> IO (FunPtr (FT_outline_ptr -> IO ()))
                        
                                            
-- end of file
