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
  
  FT_outline, -- only expose the type not the constructor..
  withOutline,
  contours,
  
) where

import Graphics.Rendering.FreeType.Internals.CBaseTypes ( 
      FT_library(..), FT_library_ptr, FT_outline(..), FT_struct_outline  )
-- import Graphics.Rendering.FreeType.Internals.CBasicDataTypes
import Graphics.Rendering.FreeType.Internals.COutline
import Graphics.Rendering.FreeType.Internals.Wrappers 
import Graphics.Rendering.FreeType.Utils ( nullForeignPtr )


import Control.Exception ( bracket )
import Foreign.ForeignPtr ( newForeignPtr, withForeignPtr, finalizeForeignPtr )
import Foreign.Marshal.Alloc ( alloca )
import Foreign.Ptr ( Ptr )




-------------------------------------------------------------------------------- 

isNullFT_outline :: FT_outline -> IO Bool
isNullFT_outline (FT_outline lib) = nullForeignPtr lib

withOutline :: FT_library -> Int -> Int -> a -> (FT_outline -> IO a) -> IO a
withOutline lib max_pts max_cts failureValue action = 
  bracket (newOutline lib max_pts max_cts) doneOutline 
          (\ftoln -> do { check_null <- isNullFT_outline ftoln 
                        ; if check_null
                             then do putStrLn "withOutline: failed"
                                     return failureValue
                             else action ftoln })  
                             
newOutline :: FT_library -> Int -> Int -> IO FT_outline
newOutline (FT_library lib) max_pts max_cts  = 
    withForeignPtr lib $ \h ->
        alloca $ \ptr -> do 
        ec <- ft_outline_new h (fromIntegral max_pts) (fromIntegral max_cts) ptr
        case ec of
          0 -> do fin <- mkDoneOutline (freeOutline_ h)
                  p   <- newForeignPtr fin ptr
                  return (FT_outline p)
          _ -> fail ("newOutline: failed to initialize, error " ++ show ec)



  

doneOutline :: FT_outline -> IO ()
doneOutline (FT_outline h) = finalizeForeignPtr h


-- doneFace_ drops the return code as per doneLibrary_. 
freeOutline_ :: FT_library_ptr -> Ptr FT_struct_outline -> IO ()
freeOutline_ h o = ft_outline_done h o >> return ()

contours :: FT_outline -> IO Int
contours (FT_outline oln) = 
      withForeignPtr oln $ \h -> do
          i <- peekOutline_n_contours h
          return (fromIntegral i)
          
          
          
          
