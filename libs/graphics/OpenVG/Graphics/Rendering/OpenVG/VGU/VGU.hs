{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenVG.VGU.VGU
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- This module corresponds to section 17 (The VGU Utility Library) 
-- of the OpenVG 1.1 specs.
--
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenVG.VGU.VGU  where

data VGU_ErrorCode = 
     VGU_NoError
   | VGU_BadHandleError
   | VGU_IllegalArgumentError
   | VGU_OutOfMemoryError
   | VGU_PathCapabilityError
   | VGU_BadWarpError
   deriving ( Eq, Ord, Show )

