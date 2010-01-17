{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenVG.VG.Extending
-- Copyright   :  (c) Stephen Tetley 2008-2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  unstable
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


import Graphics.Rendering.OpenVG.Raw.VG.Core101 ( VGenum )
import Graphics.Rendering.OpenVG.Raw.VG.Extending

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
-- 
-- 'stringId' wraps the OpenVG function vgGetString:
--
-- > const VGubyte * vgGetString(VGStringID name)
--
stringId :: StringID -> GettableStateVar String
stringId sid = makeGettableStateVar $ do 
    cstr <- vgGetString (marshalStringID sid)
    ans  <- peekCString cstr
    return ans



--------------------------------------------------------------------------------
   
marshalStringID :: StringID -> VGenum
marshalStringID x = case x of 
    Vendor     -> vg_VENDOR
    Renderer   -> vg_RENDERER
    Version    -> vg_VERSION
    Extensions -> vg_EXTENSIONS
   
