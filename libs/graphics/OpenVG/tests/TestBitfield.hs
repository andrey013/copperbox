{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  TestBitfield
-- Copyright   :  (c) Stephen Tetley 2008, 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Utility functions.
--
--
--------------------------------------------------------------------------------

module TestBitfield where

import Graphics.Rendering.OpenVG.VG.BasicTypes 
import Graphics.Rendering.OpenVG.VG.Constants
import Graphics.Rendering.OpenVG.VG.Utils


demo1 = unbits32 unmarshalPathCapabilities 3 -- [AppendFrom, AppendTo]
demo2 = unbits32 unmarshalPathCapabilities $ 
          bitwiseOr marshalPathCapabilities [AppendFrom, AppendTo]


--------------------------------------------------------------------------------

-- | PathCapabilities specify which operations may be performed on a 
-- given path.
data PathCapabilities = 
     AppendFrom
   | AppendTo
   | Modify
   | TransformFrom
   | TransformTo
   | InterpolateFrom
   | InterpolateTo
   | PathLength
   | PointAlongPath
   | TangentAlongPath
   | PathBounds
   | PathTransfomedBounds
   | CapabilityAll
   deriving ( Eq, Ord, Show )

      
marshalPathCapabilities :: PathCapabilities -> VGenum
marshalPathCapabilities x = case x of 
    AppendFrom           -> vg_PATH_CAPABILITY_APPEND_FROM
    AppendTo             -> vg_PATH_CAPABILITY_APPEND_TO
    Modify               -> vg_PATH_CAPABILITY_MODIFY
    TransformFrom        -> vg_PATH_CAPABILITY_TRANSFORM_FROM
    TransformTo          -> vg_PATH_CAPABILITY_TRANSFORM_TO
    InterpolateFrom      -> vg_PATH_CAPABILITY_INTERPOLATE_FROM
    InterpolateTo        -> vg_PATH_CAPABILITY_INTERPOLATE_TO
    PathLength           -> vg_PATH_CAPABILITY_PATH_LENGTH
    PointAlongPath       -> vg_PATH_CAPABILITY_POINT_ALONG_PATH
    TangentAlongPath     -> vg_PATH_CAPABILITY_TANGENT_ALONG_PATH
    PathBounds           -> vg_PATH_CAPABILITY_PATH_BOUNDS
    PathTransfomedBounds -> vg_PATH_CAPABILITY_PATH_TRANSFORMED_BOUNDS
    CapabilityAll        -> vg_PATH_CAPABILITY_ALL


unmarshalPathCapabilities :: VGenum -> PathCapabilities 
unmarshalPathCapabilities x
    | x == vg_PATH_CAPABILITY_APPEND_FROM             = AppendFrom
    | x == vg_PATH_CAPABILITY_APPEND_TO               = AppendTo 
    | x == vg_PATH_CAPABILITY_MODIFY                  = Modify 
    | x == vg_PATH_CAPABILITY_TRANSFORM_FROM          = TransformFrom 
    | x == vg_PATH_CAPABILITY_TRANSFORM_TO            = TransformTo 
    | x == vg_PATH_CAPABILITY_INTERPOLATE_FROM        = InterpolateFrom 
    | x == vg_PATH_CAPABILITY_INTERPOLATE_TO          = InterpolateTo 
    | x == vg_PATH_CAPABILITY_PATH_LENGTH             = PathLength 
    | x == vg_PATH_CAPABILITY_POINT_ALONG_PATH        = PointAlongPath 
    | x == vg_PATH_CAPABILITY_TANGENT_ALONG_PATH      = TangentAlongPath 
    | x == vg_PATH_CAPABILITY_PATH_BOUNDS             = PathBounds 
    | x == vg_PATH_CAPABILITY_PATH_TRANSFORMED_BOUNDS = PathTransfomedBounds 
    | x == vg_PATH_CAPABILITY_ALL                     = CapabilityAll 
    | otherwise = error ("unmarshalPathCapabilities: illegal value " ++ show x)



