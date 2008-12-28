{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenVG.VG.Scissoring
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- This module corresponds to section 7 (Scissoring, Masking and Clearing) 
-- of the OpenVG 1.1 specs.
--
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenVG.VG.Scissoring  where

import Graphics.Rendering.OpenVG.VG.BasicTypes ( VGenum )
import Graphics.Rendering.OpenVG.Constants (
    vg_CLEAR_MASK, vg_FILL_MASK, vg_SET_MASK, vg_UNION_MASK, 
    vg_INTERSECT_MASK, vg_SUBTRACT_MASK )  
    
data MaskOperation =
     ClearMask
   | FillMask
   | SetMask
   | UnionMask
   | IntersectMask
   | SubtractMask
   deriving ( Eq, Ord, Show )

marshalMaskOperation :: MaskOperation -> VGenum
marshalMaskOperation x = case x of
    ClearMask -> vg_CLEAR_MASK
    FillMask -> vg_FILL_MASK
    SetMask -> vg_SET_MASK
    UnionMask -> vg_UNION_MASK
    IntersectMask -> vg_INTERSECT_MASK
    SubtractMask -> vg_SUBTRACT_MASK
