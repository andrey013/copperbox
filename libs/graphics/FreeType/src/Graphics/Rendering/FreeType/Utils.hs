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

module Graphics.Rendering.FreeType.Utils (
  
  nullForeignPtr
  
) where

import Foreign.ForeignPtr ( ForeignPtr, withForeignPtr )
import Foreign.Ptr ( nullPtr )


-------------------------------------------------------------------------------- 

nullForeignPtr :: ForeignPtr a -> IO Bool
nullForeignPtr fptr = 
  withForeignPtr fptr $ \p -> return $ p == nullPtr
          
          
          
          
