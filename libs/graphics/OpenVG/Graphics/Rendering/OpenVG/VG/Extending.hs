{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenVG.VG.Extending
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- This module corresponds to section 15 (Extending the API) 
-- of the OpenVG 1.1 specs.
--
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenVG.VG.Extending (
  StringID(..), 
  getString,
) where

import Graphics.Rendering.OpenVG.VG.BasicTypes ( VGenum )
import Graphics.Rendering.OpenVG.VG.CFunDecls ( vgGetString ) 
        
import Graphics.Rendering.OpenVG.VG.Constants (
    vg_VENDOR, vg_RENDERER, vg_VERSION, vg_EXTENSIONS ) 

import Foreign.C.String ( peekCString )
    
data StringID =
     Vendor
   | Renderer
   | Version
   | Extensions
   deriving ( Eq, Ord, Show )
   
getString :: StringID -> IO (Either () String)
getString sid = do 
    cstr <- vgGetString (marshalStringID sid)
    ans <- peekCString cstr
    return (Right ans)


   
marshalStringID :: StringID -> VGenum
marshalStringID x = case x of 
    Vendor -> vg_VENDOR
    Renderer -> vg_RENDERER
    Version -> vg_VERSION
    Extensions -> vg_EXTENSIONS
   
