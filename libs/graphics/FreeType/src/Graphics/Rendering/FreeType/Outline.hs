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
  
  -- * initialize and free a FreeType /library/.

) where


import Graphics.Rendering.FreeType.Internals.CBasicDataTypes
import Graphics.Rendering.FreeType.Internals.COutline
import Graphics.Rendering.FreeType.Internals.Wrappers 

{-
import Foreign.C.String ( withCString )
import Foreign.ForeignPtr ( newForeignPtr, finalizeForeignPtr, 
    withForeignPtr )
import Foreign.Marshal.Alloc ( alloca )
-- import Foreign.Ptr ( Ptr )
import Foreign.Storable ( peek )
-}


-------------------------------------------------------------------------------- 


    
  
-- newOutline :: FTlibrary -> Int -> Int -> IO Outline 
