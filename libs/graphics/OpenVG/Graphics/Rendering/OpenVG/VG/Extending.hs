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

module Graphics.Rendering.OpenVG.VG.Extending where

import Graphics.Rendering.OpenVG.VG.BasicTypes ( VGenum )
import Graphics.Rendering.OpenVG.Constants (
    vg_ID_VENDOR, vg_ID_RENDERER, vg_ID_VERSION, vg_ID_EXTENSIONS ) 
    
data StringID =
     IDVendor
   | IDRenderer
   | IDVersion
   | IDExtensions
   deriving ( Eq, Ord, Show )
   
marshalStringID :: StringID -> VGenum
marshalStringID x = case x of 
    IDVendor -> vg_ID_VENDOR
    IDRenderer -> vg_ID_RENDERER
    IDVersion -> vg_ID_VERSION
    IDExtensions -> vg_ID_EXTENSIONS
   
