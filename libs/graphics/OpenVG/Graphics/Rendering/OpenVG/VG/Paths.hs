{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenVG.VG.Paths
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- This module corresponds to section 8 (Paths) 
-- of the OpenVG 1.0.1 specs.
--
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenVG.VG.Paths (
  PaintMode(..), marshalPaintMode,
  createPath, clearPath, destroyPath,
  removePathCapabilities, getPathCapabilities,
  appendPath
) where


import Graphics.Rendering.OpenVG.VG.BasicTypes ( 
    VGenum, VGint, VGfloat, VGPath )
import Graphics.Rendering.OpenVG.VG.CFunDecls ( 
    vgCreatePath, vgClearPath, vgDestroyPath, 
    vgRemovePathCapabilities, vgGetPathCapabilities, 
    vgAppendPath )
import Graphics.Rendering.OpenVG.VG.Constants ( 
    vg_PATH_DATATYPE_S_8, vg_PATH_DATATYPE_S_16,
    vg_PATH_DATATYPE_S_32, vg_PATH_DATATYPE_F,
    
    vg_PATH_CAPABILITY_APPEND_FROM, vg_PATH_CAPABILITY_APPEND_TO,
    vg_PATH_CAPABILITY_MODIFY, vg_PATH_CAPABILITY_TRANSFORM_FROM,
    vg_PATH_CAPABILITY_TRANSFORM_TO, vg_PATH_CAPABILITY_INTERPOLATE_FROM, 
    vg_PATH_CAPABILITY_INTERPOLATE_TO, vg_PATH_CAPABILITY_PATH_LENGTH, 
    vg_PATH_CAPABILITY_POINT_ALONG_PATH, vg_PATH_CAPABILITY_TANGENT_ALONG_PATH, 
    vg_PATH_CAPABILITY_PATH_BOUNDS, vg_PATH_CAPABILITY_PATH_TRANSFORMED_BOUNDS,
    vg_PATH_CAPABILITY_ALL, 
    
    vg_PATH_FORMAT, vg_PATH_DATATYPE, vg_PATH_SCALE, vg_PATH_BIAS, 
    vg_PATH_NUM_SEGMENTS, vg_PATH_NUM_COORDS,
    vg_CAP_BUTT, vg_CAP_ROUND, vg_CAP_SQUARE,
    vg_JOIN_MITER, vg_JOIN_ROUND, vg_JOIN_BEVEL,
    vg_EVEN_ODD, vg_NON_ZERO,
    vg_STROKE_PATH, vg_FILL_PATH )
import Graphics.Rendering.OpenVG.VG.Utils ( 
    Marshal(..), Unmarshal(..), bitwiseOr, unbits )

data PathDatatype =
     S_8
   | S_16
   | S_32
   | F_ieee754
   deriving ( Eq, Ord, Show )
   
data PathType = 
     Absolute
   | Relative
   deriving ( Eq, Ord, Show )
      
data PathSegment = 
     ClosePath
   | MoveTo
   | LineTo
   | HLineTo
   | VLineTo
   | QuadTo
   | CubicTo
   | SQuadTo
   | SCubicTo
   | SCCWArcTo
   | SCWArcTo
   | LCCWArcTo
   | LCWArcTo
   deriving ( Eq, Ord, Show )
   
data PathCommand = 
     MoveToAbs
   | MoveToRel
   | LineToAbs
   | LineToRel
   | HLineToAbs
   | HLineToRel
   | VLineToAbs
   | VLineToRel
   | QuadToAbs
   | QuadToRel
   | CubicToAbs
   | CubicToRel
   | SQuadToAbs
   | SQuadToRel
   | SCubicToAbs
   | SCubicToRel
   | SCCWArcToAbs
   | SCCWArcToRel
   | SCWArcToAbs
   | SCWArcToRel
   | LCCWArcToAbs
   | LCCWArcToRel
   | LCWArcToAbs
   | LCWArcToRel
   deriving ( Eq, Ord, Show )

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
   
data PathParamType = 
     PathParamFormat
   | PathParamDatatype
   | PathParamScale
   | PathParamBias
   | PathParamNumSegments
   | PathParamNumCoords
   deriving ( Eq, Ord, Show )
   
data CapStyle = 
     CapButt
   | CapRound
   | CapSquare
   deriving ( Eq, Ord, Show )
   
data JoinStyle = 
     JoinMiter
   | JoinRound
   | JoinBevel
   deriving ( Eq, Ord, Show )
   
data FillRule = 
     EvenOdd
   | NonZero
   deriving ( Eq, Ord, Show )

data PaintMode =
     StrokePath
   | FillPath
   deriving ( Eq, Ord, Show )


createPath :: VGint -> PathDatatype -> VGfloat -> VGfloat
                 -> VGint -> VGint -> [PathCapabilities] -> IO VGPath
createPath fmt typ scale bias sch cch cs = 
    vgCreatePath fmt (marshalPathDatatype typ) scale bias sch cch (bitwiseOr cs)

clearPath :: VGPath -> [PathCapabilities] -> IO ()
clearPath h cs = vgClearPath h (bitwiseOr cs)

destroyPath :: VGPath -> IO ()
destroyPath = vgDestroyPath

removePathCapabilities :: VGPath -> [PathCapabilities] -> IO ()
removePathCapabilities h cs = vgRemovePathCapabilities h (bitwiseOr cs)

getPathCapabilities :: VGPath -> IO [PathCapabilities]
getPathCapabilities h = do 
    b <- vgGetPathCapabilities h
    return $ unbits b

appendPath :: VGPath -> VGPath -> IO ()
appendPath = vgAppendPath


--------------------------------------------------------------------------------
marshalPathDatatype :: PathDatatype -> VGenum
marshalPathDatatype x = case x of
    S_8 -> vg_PATH_DATATYPE_S_8
    S_16 -> vg_PATH_DATATYPE_S_16
    S_32 -> vg_PATH_DATATYPE_S_32
    F_ieee754 -> vg_PATH_DATATYPE_F
   
   
marshalPathCapabilities :: PathCapabilities -> VGenum
marshalPathCapabilities x = case x of 
    AppendFrom -> vg_PATH_CAPABILITY_APPEND_FROM
    AppendTo -> vg_PATH_CAPABILITY_APPEND_TO
    Modify -> vg_PATH_CAPABILITY_MODIFY
    TransformFrom -> vg_PATH_CAPABILITY_TRANSFORM_FROM
    TransformTo -> vg_PATH_CAPABILITY_TRANSFORM_TO
    InterpolateFrom -> vg_PATH_CAPABILITY_INTERPOLATE_FROM
    InterpolateTo -> vg_PATH_CAPABILITY_INTERPOLATE_TO
    PathLength -> vg_PATH_CAPABILITY_PATH_LENGTH
    PointAlongPath -> vg_PATH_CAPABILITY_POINT_ALONG_PATH
    TangentAlongPath -> vg_PATH_CAPABILITY_TANGENT_ALONG_PATH
    PathBounds -> vg_PATH_CAPABILITY_PATH_BOUNDS
    PathTransfomedBounds -> vg_PATH_CAPABILITY_PATH_TRANSFORMED_BOUNDS
    CapabilityAll -> vg_PATH_CAPABILITY_ALL

instance Marshal PathCapabilities where marshal = marshalPathCapabilities     

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
        
instance Unmarshal PathCapabilities where unmarshal = unmarshalPathCapabilities
  
marshalPathParamType :: PathParamType -> VGenum
marshalPathParamType x = case x of
    PathParamFormat -> vg_PATH_FORMAT
    PathParamDatatype -> vg_PATH_DATATYPE
    PathParamScale -> vg_PATH_SCALE
    PathParamBias -> vg_PATH_BIAS
    PathParamNumSegments -> vg_PATH_NUM_SEGMENTS
    PathParamNumCoords -> vg_PATH_NUM_COORDS


marshalCapStyle :: CapStyle -> VGenum
marshalCapStyle x = case x of
    CapButt -> vg_CAP_BUTT
    CapRound -> vg_CAP_ROUND
    CapSquare -> vg_CAP_SQUARE
    
marshalJoinStyle :: JoinStyle -> VGenum
marshalJoinStyle x = case x of
    JoinMiter -> vg_JOIN_MITER
    JoinRound -> vg_JOIN_ROUND 
    JoinBevel -> vg_JOIN_BEVEL

marshalFillRule :: FillRule -> VGenum
marshalFillRule x = case x of 
    EvenOdd -> vg_EVEN_ODD
    NonZero -> vg_NON_ZERO

marshalPaintMode :: PaintMode -> VGenum     
marshalPaintMode x = case x of 
    StrokePath -> vg_STROKE_PATH
    FillPath -> vg_FILL_PATH

instance Marshal PaintMode where marshal = marshalPaintMode
 