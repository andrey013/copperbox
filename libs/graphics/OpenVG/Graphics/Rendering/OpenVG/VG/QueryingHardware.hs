{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenVG.VG.QueryingHardware
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- This module corresponds to section 14 (Querying Hardware Capabilities) 
-- of the OpenVG 1.1 specs.
--
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenVG.VG.QueryingHardware where

import Graphics.Rendering.OpenVG.VG.BasicTypes ( VGenum )
import Graphics.Rendering.OpenVG.VG.Constants (
    vg_IMAGE_FORMAT_QUERY, vg_PATH_DATATYPE_QUERY, 
    vg_HARDWARE_ACCELERATED, vg_HARDWARE_UNACCELERATED )

data HardwareQueryType = 
     ImageFormatQuery
   | PathDatatypeQuery
   deriving ( Eq, Ord, Show )
   
data HardwareQueryResult = 
     HardwareAccelerated
   | HardwareUnccelerated
   deriving ( Eq, Ord, Show )
   
marshalHardwareQueryType :: HardwareQueryType -> VGenum
marshalHardwareQueryType x = case x of
    ImageFormatQuery -> vg_IMAGE_FORMAT_QUERY
    PathDatatypeQuery -> vg_PATH_DATATYPE_QUERY 
   
marshalHardwareQueryResult :: HardwareQueryResult -> VGenum
marshalHardwareQueryResult x = case x of
    HardwareAccelerated -> vg_HARDWARE_ACCELERATED
    HardwareUnccelerated -> vg_HARDWARE_UNACCELERATED

