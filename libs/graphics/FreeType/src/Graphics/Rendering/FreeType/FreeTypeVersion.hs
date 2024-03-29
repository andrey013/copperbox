{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.FreeType.GlyphManagement
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

module Graphics.Rendering.FreeType.FreeTypeVersion (

  -- * FreeType library version number
  VersionNumber, 
  libraryVersion,
  
) where  
  
import Graphics.Rendering.FreeType.Internals.CBaseTypes ( FT_library(..) )
import Graphics.Rendering.FreeType.Internals.CFreeTypeVersion


import Foreign.ForeignPtr ( withForeignPtr )
import Foreign.Marshal.Alloc ( alloca )
import Foreign.Storable ( peek )

--------------------------------------------------------------------------------
-- FreeType library version number


type VersionNumber = (Int,Int,Int)

libraryVersion :: FT_library -> IO VersionNumber
libraryVersion lib = ftLibraryVersion lib

ftLibraryVersion :: FT_library -> IO VersionNumber
ftLibraryVersion (FT_library lib) =
  withForeignPtr lib $ \h -> 
    alloca $ \ptr_vmj -> do 
        alloca $ \ptr_vmn -> do 
            alloca $ \ptr_vph -> do                     
                ft_library_version h ptr_vmj ptr_vmn ptr_vph
                
                vmj <- peek ptr_vmj
                vmn <- peek ptr_vmn
                vph <- peek ptr_vph
                
                return (fromIntegral vmj, fromIntegral vmn, fromIntegral vph)

