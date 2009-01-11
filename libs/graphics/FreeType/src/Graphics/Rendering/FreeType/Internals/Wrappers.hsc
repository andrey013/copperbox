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




import Graphics.Rendering.FreeType.Internals.CBasicDataTypes

import Foreign.Ptr ( FunPtr ) 

--------------------------------------------------------------------------------

foreign import ccall "wrapper"
    mkDoneLibrary :: (FT_Library -> IO ()) 
                  -> IO (FunPtr (FT_Library -> IO ()))
  
foreign import ccall "wrapper"
    mkDoneFace :: (FT_Face -> IO ()) 
                  -> IO (FunPtr (FT_Face -> IO ()))
                    
-- end of file
