{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenVG.VG.Extending
-- Copyright   :  (c) Stephen Tetley 2008, 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- This module corresponds to section 14 (Extending the API) 
-- of the OpenVG 1.0.1 specs.
--
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenVG.VG.Extending (
  -- * Accessing extensions dynamically
  StringID(..), 
  stringId,
) where

import Graphics.Rendering.OpenVG.VG.BasicTypes ( VGenum )
import Graphics.Rendering.OpenVG.VG.CFunDecls ( vgGetString ) 
import Graphics.Rendering.OpenVG.VG.Constants (
    vg_VENDOR, vg_RENDERER, vg_VERSION, vg_EXTENSIONS ) 

import Data.StateVar (
   GettableStateVar, makeGettableStateVar )
   
import Foreign.C.String ( peekCString )

--------------------------------------------------------------------------------
-- Accessing extensions dynamically
    
data StringID =
     Vendor
   | Renderer
   | Version
   | Extensions
   deriving ( Eq, Ord, Show )



-- | Query the OpenVG implementation.   
stringId :: StringID -> GettableStateVar String
stringId sid = makeGettableStateVar $ do 
    cstr <- vgGetString (marshalStringID sid)
    ans  <- peekCString cstr
    return ans

--------------------------------------------------------------------------------
   
marshalStringID :: StringID -> VGenum
marshalStringID x = case x of 
    Vendor -> vg_VENDOR
    Renderer -> vg_RENDERER
    Version -> vg_VERSION
    Extensions -> vg_EXTENSIONS
   
