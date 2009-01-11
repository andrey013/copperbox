{-# LANGUAGE ForeignFunctionInterface   #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.FreeType.BaseInterface
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Binding to some of the FreeType2 library.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.FreeType.BaseInterface (
  module Graphics.Rendering.FreeType.Internals.CBaseInterface,  
  module Graphics.Rendering.FreeType.Internals.CBasicDataTypes,
--  module Graphics.Rendering.FreeType.Internals.CImage,
--  module Graphics.Rendering.FreeType.Internals.COutline,

  initFreeType,
  doneFreeType,
  hackExtract,
) where

import Graphics.Rendering.FreeType.Internals.CBaseInterface
import Graphics.Rendering.FreeType.Internals.CBasicDataTypes
-- import Graphics.Rendering.FreeType.Internals.CImage
-- import Graphics.Rendering.FreeType.Internals.COutline


import Foreign.ForeignPtr
import Foreign.Marshal.Alloc ( alloca )
import Foreign.Ptr ( Ptr, FunPtr )
import Foreign.Storable ( peek )

doneLibrary :: Ptr FTLIBRARY_ -> IO ()
doneLibrary p = ft_done_freetype p >> return ()

foreign import ccall "wrapper"
    mkDoneLibrary :: (Ptr FTLIBRARY_ -> IO ()) 
                  -> IO (FunPtr (Ptr FTLIBRARY_ -> IO ()))

 
  
 
initFreeType :: IO FTlibrary
initFreeType =
  alloca $ \ptrptr -> do 
  ec <- ft_init_freetype ptrptr
  case ec of
    0 -> do fin <- mkDoneLibrary doneLibrary
            ptr <- peek ptrptr
            p <- newForeignPtr fin ptr
            return (FTlibrary p)
    _ -> fail ("initFreeType: failed to open " ++ show ec)

doneFreeType :: FTlibrary -> IO ()
doneFreeType (FTlibrary h) = finalizeForeignPtr h



hackExtract :: FTlibrary -> IO (Ptr FTLIBRARY_)
hackExtract (FTlibrary p) = return $ unsafeForeignPtrToPtr p

