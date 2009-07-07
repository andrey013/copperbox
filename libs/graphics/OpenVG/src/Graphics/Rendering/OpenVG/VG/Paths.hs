{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenVG.VG.Paths
-- Copyright   :  (c) Stephen Tetley 2008, 2009
-- License     :  BSD3
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
  -- * Datatypes
  PathDatatype(..),
  PathType(..),
  PathCommand(..),
  
  
  -- * Creating and destroying paths
  PathCapabilities(..),
  withPath,
  createPath, clearPath, destroyPath,

  -- * Path queries
  format, datatype, pathScale, bias, numSegments, numCoords,

  -- * Querying and modifying path capabilities
  getPathCapabilities, 
  removePathCapabilities, 
  
  -- * Copying data between paths
  appendPath,
  
  -- * Appending client-side data to a path.
  StorablePathData, -- don't export the member as the instances are fixed
  appendPathData,
  
  -- * Modifying path data
  modifyPathCoords,
  
  -- * Transforming a path
  transformPath,
  
  -- * Interpolating between paths
  interpolatePath,
  
  -- * Setting stroke parameters
  lineWidth,
  CapStyle(..), capStyle,
  JoinStyle(..), joinStyle,
  miterLimit,
  maxDashCount, 
  dashPattern, disableDashPattern,
  dashPhase,
  dashPhaseReset,
  
  -- * Filling or stroking a path
  FillRule(..),  fillRule,
  PaintMode(..), marshalPaintMode,
  drawPath,
  fillPath,
  strokePath,
  fillStrokePath
) where


import Graphics.Rendering.OpenVG.VG.BasicTypes ( 
    VGenum, VGint, VGfloat, VGPath, marshalBool, unmarshalBool )
import Graphics.Rendering.OpenVG.VG.CFunDecls ( 
    vgCreatePath, vgClearPath, vgDestroyPath, 
    vgRemovePathCapabilities, vgGetPathCapabilities, 
    vgAppendPath, vgAppendPathData, vgModifyPathCoords,
    vgTransformPath, vgInterpolatePath,    
    vgDrawPath )
import Graphics.Rendering.OpenVG.VG.Constants ( 
    vg_PATH_DATATYPE_S_8, vg_PATH_DATATYPE_S_16,
    vg_PATH_DATATYPE_S_32, vg_PATH_DATATYPE_F,
    vg_PATH_FORMAT_STANDARD,
    
    vg_CLOSE_PATH, 
    vg_MOVE_TO_ABS, vg_MOVE_TO_REL,
    vg_LINE_TO_ABS, vg_LINE_TO_REL,
    vg_HLINE_TO_ABS, vg_HLINE_TO_REL,
    vg_VLINE_TO_ABS, vg_VLINE_TO_REL,
    vg_QUAD_TO_ABS, vg_QUAD_TO_REL,
    vg_CUBIC_TO_ABS, vg_CUBIC_TO_REL,
    vg_SQUAD_TO_ABS, vg_SQUAD_TO_REL,
    vg_SCUBIC_TO_ABS, vg_SCUBIC_TO_REL,
    vg_SCCWARC_TO_ABS, vg_SCCWARC_TO_REL,
    vg_SCWARC_TO_ABS, vg_SCWARC_TO_REL,
    vg_LCCWARC_TO_ABS, vg_LCCWARC_TO_REL,
    vg_LCWARC_TO_ABS, vg_LCWARC_TO_REL,
    
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
    vg_STROKE_PATH, vg_FILL_PATH  )
import Graphics.Rendering.OpenVG.VG.Parameters ( 
    ParamType ( FillRule, 
                StrokeLineWidth, StrokeCapStyle,
                StrokeJoinStyle, StrokeMiterLimit, 
                StrokeDashPattern, 
                StrokeDashPhase, StrokeDashPhaseReset,
                MaxDashCount ),
    getParameteri, getParameterf, seti, setf, setfv, geti )     
    
import Graphics.Rendering.OpenVG.VG.Utils ( 
    Marshal(..), Unmarshal(..), unmarshalIntegral, enumValue, 
    bitwiseOr, unbits )

import Graphics.Rendering.OpenGL.GL.StateVar (
    SettableStateVar, makeSettableStateVar,
    GettableStateVar, makeGettableStateVar,
    ( $= ) ) 

import Data.Int ( Int8, Int16, Int32 )
import Foreign.Marshal.Array ( newArray )
import Foreign.Storable ( Storable )

--------------------------------------------------------------------------------
-- Datatypes

-- | @PathDatatype@ defines the permissible numeric types for path 
-- coordinate data.     
data PathDatatype =
     Int8
   | Int16
   | Int32
   | Float
   deriving ( Eq, Ord, Show )

-- | @PathType@ corresponds to the OpenVG enumeration @VGPathAbsRel@. 
data PathType = 
      Absolute
   | Relative
   deriving ( Eq, Ord, Show )

-- There is no Haskell equivalent to @VGPathSegment@. 
-- It is subsumed by PathCommand 

-- | @PathCommand@ corresponds to the OpenVG enumeration @VGPathCommand@,
-- but includes ClosePath aka @VG_CLOSE_PATH@.      
data PathCommand = 
     ClosePath
   | MoveToAbs
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



--------------------------------------------------------------------------------
-- Creating and destroying paths

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


-- | @withPath@ - create a path, run an action on it, destroy the path.
withPath :: PathDatatype -> VGfloat -> VGfloat
                 -> VGint -> VGint -> [PathCapabilities]
                 -> (VGPath -> IO a) -> IO a
withPath typ scl bi sch cch cs action = do
    path  <- createPath typ scl bi sch cch cs
    ans   <- action path
    destroyPath path
    return ans
          

-- | @createPath@ - corresponds to the OpenVG function @vgCreatePath@.
-- @createPath@ can only create paths in the standard format 
-- (VG_PATH_FORMAT_STANDARD), extensions are not supported.   
createPath :: PathDatatype -> VGfloat -> VGfloat
                 -> VGint -> VGint -> [PathCapabilities] -> IO VGPath
createPath typ scl bi sch cch cs = 
    vgCreatePath fmt (marshal typ) scl bi sch cch (bitwiseOr cs)
  where
    -- Other paths formats maybe defined as extensions, for the present
    -- restrict the format to just VG_PATH_FORMAT_STANDARD 
    fmt :: VGint
    fmt = vg_PATH_FORMAT_STANDARD

-- | @clearPath@ corresponds to the OpenVG function @vgClearPath@.         
clearPath :: VGPath -> [PathCapabilities] -> IO ()
clearPath h cs = vgClearPath h (bitwiseOr cs)

-- | @destroyPath@ corresponds to the OpenVG function @vgDestroyPath@. 
destroyPath :: VGPath -> IO ()
destroyPath = vgDestroyPath

--------------------------------------------------------------------------------
--  Path queries
  
-- | @format@ - get the path format. Currently the only supported 
-- format is @VG_PATH_FORMAT_STANDARD@ - 0. 
format :: VGPath -> GettableStateVar VGint
format h = makeGettableStateVar $
    getParameteri h vg_PATH_FORMAT

-- | @datatype@ - get the PathDatatype.
datatype :: VGPath -> GettableStateVar PathDatatype
datatype h = makeGettableStateVar $ do
    a <- getParameteri h vg_PATH_DATATYPE
    return $ unmarshalIntegral a

-- | @pathScale@ - get the scaling factor of the path.    
pathScale :: VGPath -> GettableStateVar VGfloat
pathScale h = makeGettableStateVar $
    getParameterf h vg_PATH_SCALE

-- | @bias@ - get the bias factor of the path. 
bias :: VGPath -> GettableStateVar VGfloat
bias h = makeGettableStateVar $
    getParameterf h vg_PATH_BIAS

-- | @numSegments@ - get the number of segments stored in the path.     
numSegments :: VGPath -> GettableStateVar VGint 
numSegments h = makeGettableStateVar $
    getParameteri h vg_PATH_NUM_SEGMENTS

-- | @numSegments@ - get the total number of coordinates stored in the path. 
numCoords :: VGPath -> GettableStateVar VGint 
numCoords h = makeGettableStateVar $
    getParameteri h vg_PATH_NUM_COORDS

   
--------------------------------------------------------------------------------
-- Path capabilities

-- | @getPathCapabilities@ corresponds to the OpenVG 
-- function @vgGetPathCapabilities@. 
getPathCapabilities :: VGPath -> IO [PathCapabilities]
getPathCapabilities h = do 
    b <- vgGetPathCapabilities h
    return $ unbits b
    
-- | @removePathCapabilities@ corresponds to the OpenVG 
-- function @vgRemovePathCapabilities@.  
removePathCapabilities :: VGPath -> [PathCapabilities] -> IO ()
removePathCapabilities h cs = vgRemovePathCapabilities h (bitwiseOr cs)

--------------------------------------------------------------------------------
-- Copying data between paths

-- | @appendPath@ corresponds to the OpenVG function @vgAppendPath@.  
appendPath :: VGPath -> VGPath -> IO ()
appendPath = vgAppendPath


--------------------------------------------------------------------------------
-- Appending client-side data to a path


class Storable a => StorablePathData a 

instance StorablePathData Int8
instance StorablePathData Int16
instance StorablePathData Int32
instance StorablePathData VGfloat
    
-- | @appendPathData@ - TODO is this implementation valid?
appendPathData :: StorablePathData a => VGPath -> [PathCommand] -> [a] -> IO ()
appendPathData h cs ds = do 
    cmd_arr <- newArray (map (fromIntegral . marshal) cs)
    d_arr   <- newArray ds
    vgAppendPathData h (fromIntegral $ length cs) cmd_arr d_arr

    
--------------------------------------------------------------------------------
-- Modifying path data

-- | @modifyPathCoords@ corresponds to the OpenVG 
-- function @vgModifyPathCoords@. 
modifyPathCoords :: StorablePathData a => VGPath -> VGint -> [a] -> IO ()
modifyPathCoords h start ds = do
    d_arr   <- newArray ds
    vgModifyPathCoords h start (fromIntegral $ length ds) d_arr
    
--------------------------------------------------------------------------------
-- Transforming a path

-- | @transformPath@ corresponds to the OpenVG function @vgTransformPath@.
transformPath :: VGPath -> VGPath -> IO ()
transformPath = vgTransformPath

--------------------------------------------------------------------------------
-- Querying the bounding box of a path

-- pathBounds __ TODO 
                 
-- pathTransformedBounds __ TODO 

--------------------------------------------------------------------------------
-- Interpolating between paths
interpolatePath :: VGPath -> VGPath -> VGPath -> VGfloat -> IO Bool
interpolatePath dst start end amount = do 
    a <- vgInterpolatePath dst start end amount
    return $ unmarshalBool a
  
        
--------------------------------------------------------------------------------
-- Setting stroke parameters

-- | Set the line width.
lineWidth :: SettableStateVar VGfloat
lineWidth = makeSettableStateVar $ \a -> 
    setf StrokeLineWidth a

-- | @CapStyle@ corresponds to the OpenVG enumeration @VGCapStyle@.   
data CapStyle = 
     CButt
   | CRound
   | CSquare
   deriving ( Eq, Ord, Show )

   
-- | Set the end cap style.
capStyle :: SettableStateVar CapStyle
capStyle = makeSettableStateVar $ \a -> 
    seti StrokeCapStyle (enumValue a)

-- | @JoinStyle@ corresponds to the OpenVG enumeration @VGJoinStyle@.  
data JoinStyle = 
     JMiter
   | JRound
   | JBevel
   deriving ( Eq, Ord, Show )
   
-- | Set the join style.    
joinStyle :: SettableStateVar JoinStyle
joinStyle = makeSettableStateVar $ \a -> 
    seti StrokeJoinStyle (enumValue a)

-- | Set the miter limit. 
miterLimit :: SettableStateVar VGfloat
miterLimit = makeSettableStateVar $ \a -> setf StrokeMiterLimit a

-- | Get the maximum dash count supported by the implementation.
maxDashCount :: GettableStateVar VGint 
maxDashCount = makeGettableStateVar $
    geti MaxDashCount

-- | Set the dash pattern.
dashPattern :: SettableStateVar [VGfloat]
dashPattern = makeSettableStateVar $ \a -> setfv StrokeDashPattern a

-- | Disable the dash pattern.
disableDashPattern :: IO ()
disableDashPattern = dashPattern $= []

-- | Set the dash phase.
dashPhase :: SettableStateVar VGfloat
dashPhase = makeSettableStateVar $ \a -> 
    setf StrokeDashPhase a

-- | Reset the dash phase.    
dashPhaseReset :: SettableStateVar Bool
dashPhaseReset = makeSettableStateVar $ \a -> 
    seti StrokeDashPhaseReset (marshalBool a)

--------------------------------------------------------------------------------    
-- Filling or stroking a path

-- | @FillRule@ corresponds to the OpenVG enumeration @VGFillRule@.   
data FillRule = 
     EvenOdd
   | NonZero
   deriving ( Eq, Ord, Show )

-- | Set the fill rule.   
fillRule :: SettableStateVar FillRule
fillRule = makeSettableStateVar $ \a -> 
    seti FillRule (fromIntegral $ marshalFillRule a)

-- | @PaintMode@ corresponds to the OpenVG enumeration @VGPaintMode@. 
data PaintMode =
     StrokePath
   | FillPath
   deriving ( Eq, Ord, Show )

-- | @drawPath@ corresponds to the OpenVG function @vgDrawPath@.    
drawPath :: VGPath -> [PaintMode] -> IO ()
drawPath h ps = vgDrawPath h (bitwiseOr ps)

-- | Fill a path.
fillPath :: VGPath -> IO ()
fillPath h = drawPath h [FillPath]

-- | Stroke a path.
strokePath :: VGPath -> IO ()
strokePath h = drawPath h [StrokePath]

-- | Fill and stroke a path.
fillStrokePath :: VGPath -> IO ()
fillStrokePath h = drawPath h [FillPath, StrokePath]
 
--------------------------------------------------------------------------------

marshalPathDatatype :: PathDatatype -> VGenum
marshalPathDatatype x = case x of
    Int8 -> vg_PATH_DATATYPE_S_8
    Int16 -> vg_PATH_DATATYPE_S_16
    Int32 -> vg_PATH_DATATYPE_S_32
    Float -> vg_PATH_DATATYPE_F

instance Marshal PathDatatype where marshal = marshalPathDatatype

unmarshalPathDatatype :: VGenum -> PathDatatype  
unmarshalPathDatatype x
    | x == vg_PATH_DATATYPE_S_8   = Int8 
    | x == vg_PATH_DATATYPE_S_16  = Int16 
    | x == vg_PATH_DATATYPE_S_32  = Int32
    | x == vg_PATH_DATATYPE_F     = Float     
    | otherwise = error ("unmarshalPathDatatype: illegal value " ++ show x)

instance Unmarshal PathDatatype where unmarshal = unmarshalPathDatatype



marshalPathCommand :: PathCommand -> VGenum
marshalPathCommand x = case x of
    ClosePath -> vg_CLOSE_PATH
    MoveToAbs -> vg_MOVE_TO_ABS
    MoveToRel -> vg_MOVE_TO_REL
    LineToAbs -> vg_LINE_TO_ABS
    LineToRel -> vg_LINE_TO_REL
    HLineToAbs -> vg_HLINE_TO_ABS
    HLineToRel -> vg_HLINE_TO_REL
    VLineToAbs -> vg_VLINE_TO_ABS
    VLineToRel -> vg_VLINE_TO_REL
    QuadToAbs -> vg_QUAD_TO_ABS
    QuadToRel -> vg_QUAD_TO_REL
    CubicToAbs -> vg_CUBIC_TO_ABS
    CubicToRel -> vg_CUBIC_TO_REL
    SQuadToAbs -> vg_SQUAD_TO_ABS
    SQuadToRel -> vg_SQUAD_TO_REL
    SCubicToAbs -> vg_SCUBIC_TO_ABS
    SCubicToRel -> vg_SCUBIC_TO_REL
    SCCWArcToAbs -> vg_SCCWARC_TO_ABS
    SCCWArcToRel -> vg_SCCWARC_TO_REL
    SCWArcToAbs -> vg_SCWARC_TO_ABS
    SCWArcToRel -> vg_SCWARC_TO_REL
    LCCWArcToAbs -> vg_LCCWARC_TO_ABS
    LCCWArcToRel -> vg_LCCWARC_TO_REL
    LCWArcToAbs -> vg_LCWARC_TO_ABS
    LCWArcToRel -> vg_LCWARC_TO_REL

instance Marshal PathCommand where marshal = marshalPathCommand
      
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
 
marshalCapStyle :: CapStyle -> VGenum
marshalCapStyle x = case x of
    CButt -> vg_CAP_BUTT
    CRound -> vg_CAP_ROUND
    CSquare -> vg_CAP_SQUARE

instance Marshal CapStyle where marshal = marshalCapStyle
    
marshalJoinStyle :: JoinStyle -> VGenum
marshalJoinStyle x = case x of
    JMiter -> vg_JOIN_MITER
    JRound -> vg_JOIN_ROUND 
    JBevel -> vg_JOIN_BEVEL

instance Marshal JoinStyle where marshal = marshalJoinStyle

marshalFillRule :: FillRule -> VGenum
marshalFillRule x = case x of 
    EvenOdd -> vg_EVEN_ODD
    NonZero -> vg_NON_ZERO

marshalPaintMode :: PaintMode -> VGenum     
marshalPaintMode x = case x of 
    StrokePath -> vg_STROKE_PATH
    FillPath -> vg_FILL_PATH
    
unmarshalPaintMode :: VGenum -> PaintMode 
unmarshalPaintMode x
    | x == vg_STROKE_PATH         = StrokePath 
    | x == vg_FILL_PATH           = FillPath 
    | otherwise = error ("unmarshalPaintMode: illegal value " ++ show x)

instance Marshal PaintMode where marshal = marshalPaintMode
instance Unmarshal PaintMode where unmarshal = unmarshalPaintMode

