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
-- of the OpenVG 1.1 specs.
--
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenVG.VG.Paths  where


import Graphics.Rendering.OpenVG.VG.BasicTypes ( VGenum )
import Graphics.Rendering.OpenVG.Constants ( 
    
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


data PathDatatype =
     PathDatatypeS8
   | PathDatatypeS16
   | PathDatatypeS32
   | PathDatatypeF
   deriving ( Eq, Ord, Show )
   
data PathType = 
     PathAbsolute
   | PathRelative
   deriving ( Eq, Ord, Show )
      
data PathSegment = 
     PathSegmentClosePath
   | PathSegmentMoveTo
   | PathSegmentLineTo
   | PathSegmentHLineTo
   | PathSegmentVLineTo
   | PathSegmentQuadTo
   | PathSegmentCubicTo
   | PathSegmentSQuadTo
   | PathSegmentSCubicTo
   | PathSegmentSCCWArcTo
   | PathSegmentSCWArcTo
   | PathSegmentLCCWArcTo
   | PathSegmentLCWArcTo
   deriving ( Eq, Ord, Show )
   
data PathCommand = 
     PathCmdMoveToAbs
   | PathCmdMoveToRel
   | PathCmdLineToAbs
   | PathCmdLineToRel
   | PathCmdHLineToAbs
   | PathCmdHLineToRel
   | PathCmdVLineToAbs
   | PathCmdVLineToRel
   | PathCmdQuadToAbs
   | PathCmdQuadToRel
   | PathCmdCubicToAbs
   | PathCmdCubicToRel
   | PathCmdSQuadToAbs
   | PathCmdSQuadToRel
   | PathCmdSCubicToAbs
   | PathCmdSCubicToRel
   | PathCmdSCCWArcToAbs
   | PathCmdSCCWArcToRel
   | PathCmdSCWArcToAbs
   | PathCmdSCWArcToRel
   | PathCmdLCCWArcToAbs
   | PathCmdLCCWArcToRel
   | PathCmdLCWArcToAbs
   | PathCmdLCWArcToRel
   deriving ( Eq, Ord, Show )

data PathCapabilities = 
     PathCapabilityAppendFrom
   | PathCapabilityAppendTo
   | PathCapabilityModify
   | PathCapabilityTransformFrom
   | PathCapabilityTransformTo
   | PathCapabilityInterpolateFrom
   | PathCapabilityInterpolateTo
   | PathCapabilityPathLength
   | PathCapabilityPointAlongPath
   | PathCapabilityTangentAlongPath
   | PathCapabilityPathBounds
   | PathCapabilityPathTransfomedBounds
   | PathCapabilityAll
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
     FillEvenOdd
   | FillNonZero
   deriving ( Eq, Ord, Show )

data PaintMode =
     PaintModeStrokePath
   | PaintModeFillPath
   deriving ( Eq, Ord, Show )
   
marshalPathCapabilities :: PathCapabilities -> VGenum
marshalPathCapabilities x = case x of 
    PathCapabilityAppendFrom -> vg_PATH_CAPABILITY_APPEND_FROM
    PathCapabilityAppendTo -> vg_PATH_CAPABILITY_APPEND_TO
    PathCapabilityModify -> vg_PATH_CAPABILITY_MODIFY
    PathCapabilityTransformFrom -> vg_PATH_CAPABILITY_TRANSFORM_FROM
    PathCapabilityTransformTo -> vg_PATH_CAPABILITY_TRANSFORM_TO
    PathCapabilityInterpolateFrom -> vg_PATH_CAPABILITY_INTERPOLATE_FROM
    PathCapabilityInterpolateTo -> vg_PATH_CAPABILITY_INTERPOLATE_TO
    PathCapabilityPathLength -> vg_PATH_CAPABILITY_PATH_LENGTH
    PathCapabilityPointAlongPath -> vg_PATH_CAPABILITY_POINT_ALONG_PATH
    PathCapabilityTangentAlongPath -> vg_PATH_CAPABILITY_TANGENT_ALONG_PATH
    PathCapabilityPathBounds -> vg_PATH_CAPABILITY_PATH_BOUNDS
    PathCapabilityPathTransfomedBounds -> vg_PATH_CAPABILITY_PATH_TRANSFORMED_BOUNDS
    PathCapabilityAll -> vg_PATH_CAPABILITY_ALL
     
    
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
    FillEvenOdd -> vg_EVEN_ODD
    FillNonZero -> vg_NON_ZERO

marshalPaintMode :: PaintMode -> VGenum     
marshalPaintMode x = case x of 
    PaintModeStrokePath -> vg_STROKE_PATH
    PaintModeFillPath -> vg_FILL_PATH
