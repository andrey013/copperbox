{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenVG.VG.Paths
-- Copyright   :  (c) Stephen Tetley 2008, 2009, 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  HIGHLY UNSTABLE
-- Portability :  GHC
--
-- This module corresponds to section 8 (Paths) 
-- of the OpenVG 1.0.1 specs.
--
-- \*\* WARNING - this module is due to be changed significantly.
--
-- This is unfortunate as the module defines the most significant 
-- data types for vectors - the @Paths@. \*\*
--
--
--------------------------------------------------------------------------------


module Graphics.Rendering.OpenVG.VG.Paths (
  -- * Datatypes
  PathDatatype(..),
  PathAbsRel(..),
  PathSegment(..),
  PathCommand(..),
  
  
  -- * Creating and destroying paths
  PathCapabilities(..),
  withPath,
  createPath, 
  clearPath, 
  destroyPath,

  -- * Path queries
  format, 
  datatype, 
  pathScale, 
  bias, 
  numSegments, 
  numCoords,

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
  CapStyle(..), 
  capStyle,
  JoinStyle(..), 
  joinStyle,
  miterLimit,
  maxDashCount, 
  dashPattern, 
  disableDashPattern,
  dashPhase,
  dashPhaseReset,
  
  -- * Filling or stroking a path
  FillRule(..),
  fillRule,
  PaintMode(..),
  marshalPaintMode,
  drawPath,
  fillPath,
  strokePath,
  fillStrokePath

) where

import Graphics.Rendering.OpenVG.VG.Parameters
import Graphics.Rendering.OpenVG.VG.Utils ( 
    bitwiseOr, unbits32, unmarshalBool, marshalBool )

import Graphics.Rendering.OpenVG.Raw.VG.Core101 ( 
    VGenum, VGint, VGfloat, VGPath )
import Graphics.Rendering.OpenVG.Raw.VG.Paths

import Data.StateVar (
    SettableStateVar, makeSettableStateVar,
    GettableStateVar, makeGettableStateVar,
    ( $= ) ) 

import Control.Applicative
import Control.Monad
import Data.Int ( Int8, Int16, Int32 )
import Foreign.Marshal.Array ( newArray )
import Foreign.Storable ( Storable )

--------------------------------------------------------------------------------
-- Datatypes

-- | 'PathDatatype' enumerates the available numeric types for 
-- path coordinate data.     
--
data PathDatatype =
     Int8
   | Int16
   | Int32
   | Float
   deriving ( Eq, Ord, Show )

-- | 'PathAbsRel' enumerates the path addressing types, 
-- @absolute@ or @relative@. 
--
data PathAbsRel = 
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

-- | 'PathCommand' corresponds to the OpenVG enumeration @VGPathCommand@,
-- but includes ClosePath aka @VG_CLOSE_PATH@.      
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
withPath :: PathDatatype 
         -> VGfloat 
         -> VGfloat
         -> VGint 
         -> VGint 
         -> [PathCapabilities]
         -> (VGPath -> IO a) 
         -> IO a
withPath typ scl bi sch cch cs action = do
    path  <- createPath typ scl bi sch cch cs
    ans   <- action path
    destroyPath path
    return ans
          

-- | @createPath@ - corresponds to the OpenVG function @vgCreatePath@.
-- @createPath@ can only create paths in the standard format 
-- (VG_PATH_FORMAT_STANDARD), extensions are not supported.   
createPath :: PathDatatype 
           -> VGfloat 
           -> VGfloat
           -> VGint 
           -> VGint 
           -> [PathCapabilities] 
           -> IO VGPath
createPath typ scl bi sch cch cs = 
    vgCreatePath fmt (marshalPathDatatype typ) scl bi sch cch caps
  where
    -- Other paths formats maybe defined as extensions, for the present
    -- restrict the format to just VG_PATH_FORMAT_STANDARD 
    fmt   :: VGint
    fmt   = vg_PATH_FORMAT_STANDARD
    caps  = bitwiseOr marshalPathCapabilities cs

 
-- | @clearPath@ corresponds to the OpenVG function @vgClearPath@.         
clearPath :: VGPath -> [PathCapabilities] -> IO ()
clearPath h = vgClearPath h . bitwiseOr marshalPathCapabilities

-- | @destroyPath@ corresponds to the OpenVG function @vgDestroyPath@. 
destroyPath :: VGPath -> IO ()
destroyPath = vgDestroyPath

--------------------------------------------------------------------------------
--  Path queries
  
-- | @format@ - get the path format. Currently the only supported 
-- format is @VG_PATH_FORMAT_STANDARD@ - 0. 
format :: VGPath -> GettableStateVar VGint
format = makeGettableStateVar . flip getParameteri vg_PATH_FORMAT

-- | @datatype@ - get the PathDatatype.
datatype :: VGPath -> GettableStateVar PathDatatype
datatype = makeGettableStateVar . liftM (unmarshalPathDatatype . fromIntegral)  
                                . (getParameteri `flip` vg_PATH_DATATYPE)

-- | @pathScale@ - get the scaling factor of the path.    
pathScale :: VGPath -> GettableStateVar VGfloat
pathScale = makeGettableStateVar . flip getParameterf vg_PATH_SCALE

-- | @bias@ - get the bias factor of the path. 
bias :: VGPath -> GettableStateVar VGfloat
bias = makeGettableStateVar . flip getParameterf vg_PATH_BIAS

-- | @numSegments@ - get the number of segments stored in the path.     
numSegments :: VGPath -> GettableStateVar VGint 
numSegments = makeGettableStateVar . flip getParameteri vg_PATH_NUM_SEGMENTS

-- | @numSegments@ - get the total number of coordinates stored in the path. 
numCoords :: VGPath -> GettableStateVar VGint 
numCoords = makeGettableStateVar . flip getParameteri vg_PATH_NUM_COORDS

   
--------------------------------------------------------------------------------
-- Path capabilities

-- | @getPathCapabilities@ corresponds to the OpenVG 
-- function @vgGetPathCapabilities@. 
getPathCapabilities :: VGPath -> IO [PathCapabilities]
getPathCapabilities h = 
    unbits32 unmarshalPathCapabilities <$> vgGetPathCapabilities h
    
-- | @removePathCapabilities@ corresponds to the OpenVG 
-- function @vgRemovePathCapabilities@.  
removePathCapabilities :: VGPath -> [PathCapabilities] -> IO ()
removePathCapabilities h =
     vgRemovePathCapabilities h . bitwiseOr marshalPathCapabilities

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

data PathData = S_8     Int8
              | S_16    Int16
              | S_32    Int32
              | F_float Float
  deriving (Eq,Ord,Show)

{-
-- final ptr is @const void * pathData@ 

appendPathData :: VGPath -> VGint -> Ptr VGubyte -> Ptr a -> IO ()
appendPathData = vgAppendPathData
-}


-- | @appendPathData@ - TODO is this implementation valid?
appendPathData :: StorablePathData a => VGPath -> [PathCommand] -> [a] -> IO ()
appendPathData h cs ds = do 
    cmd_arr <- newArray (map (fromIntegral . marshalPathCommand) cs)
    d_arr   <- newArray ds
    vgAppendPathData h (fromIntegral $ length cs) cmd_arr d_arr

    
--------------------------------------------------------------------------------
-- Modifying path data

-- | @modifyPathCoords@ corresponds to the OpenVG 
-- function @vgModifyPathCoords@. 
modifyPathCoords :: StorablePathData a => VGPath -> VGint -> [a] -> IO ()
modifyPathCoords h start ds = 
    newArray ds >>= vgModifyPathCoords h start (fromIntegral $ length ds) 
    
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
interpolatePath dst start end amount =
    unmarshalBool <$> vgInterpolatePath dst start end amount
  
        
--------------------------------------------------------------------------------
-- Setting stroke parameters

-- | Set the line width.
lineWidth :: SettableStateVar VGfloat
lineWidth = makeSettableStateVar $ setf StrokeLineWidth

-- | @CapStyle@ corresponds to the OpenVG enumeration @VGCapStyle@.   
data CapStyle = 
     CButt
   | CRound
   | CSquare
   deriving ( Eq, Ord, Show )

   
-- | Set the end cap style.
capStyle :: SettableStateVar CapStyle
capStyle = makeSettableStateVar $
    seti StrokeCapStyle . fromIntegral . marshalCapStyle

-- | @JoinStyle@ corresponds to the OpenVG enumeration @VGJoinStyle@.  
data JoinStyle = 
     JMiter
   | JRound
   | JBevel
   deriving ( Eq, Ord, Show )
   
-- | Set the join style.    
joinStyle :: SettableStateVar JoinStyle
joinStyle = makeSettableStateVar $  
    seti StrokeJoinStyle . fromIntegral . marshalJoinStyle

-- | Set the miter limit. 
miterLimit :: SettableStateVar VGfloat
miterLimit = makeSettableStateVar $ setf StrokeMiterLimit

-- | Get the maximum dash count supported by the implementation.
maxDashCount :: GettableStateVar VGint 
maxDashCount = makeGettableStateVar $ geti MaxDashCount

-- | Set the dash pattern.
dashPattern :: SettableStateVar [VGfloat]
dashPattern = makeSettableStateVar $ setfv StrokeDashPattern

-- | Disable the dash pattern.
disableDashPattern :: IO ()
disableDashPattern = dashPattern $= []

-- | Set the dash phase.
dashPhase :: SettableStateVar VGfloat
dashPhase = makeSettableStateVar $ setf StrokeDashPhase

-- | Reset the dash phase.    
dashPhaseReset :: SettableStateVar Bool
dashPhaseReset = 
    makeSettableStateVar $ seti StrokeDashPhaseReset . marshalBool

--------------------------------------------------------------------------------    
-- Filling or stroking a path

-- | @FillRule@ corresponds to the OpenVG enumeration @VGFillRule@.   
data FillRule = 
     EvenOdd
   | NonZero
   deriving ( Eq, Ord, Show )

-- | Set the fill rule.   
fillRule :: SettableStateVar FillRule
fillRule = makeSettableStateVar $ seti FillRule . fromIntegral . marshalFillRule

-- | @PaintMode@ corresponds to the OpenVG enumeration @VGPaintMode@. 
data PaintMode =
     StrokePath
   | FillPath
   deriving ( Eq, Ord, Show )

-- | @drawPath@ corresponds to the OpenVG function @vgDrawPath@.    
drawPath :: VGPath -> [PaintMode] -> IO ()
drawPath h = vgDrawPath h . bitwiseOr marshalPaintMode

-- | Fill a path.
fillPath :: VGPath -> IO ()
fillPath = drawPath `flip` [FillPath]

-- | Stroke a path.
strokePath :: VGPath -> IO ()
strokePath = drawPath `flip` [StrokePath]

-- | Fill and stroke a path.
fillStrokePath :: VGPath -> IO ()
fillStrokePath = drawPath `flip` [FillPath, StrokePath]
 
--------------------------------------------------------------------------------

marshalPathDatatype :: PathDatatype -> VGenum
marshalPathDatatype x = case x of
    Int8  -> vg_PATH_DATATYPE_S_8
    Int16 -> vg_PATH_DATATYPE_S_16
    Int32 -> vg_PATH_DATATYPE_S_32
    Float -> vg_PATH_DATATYPE_F


unmarshalPathDatatype :: VGenum -> PathDatatype  
unmarshalPathDatatype x
    | x == vg_PATH_DATATYPE_S_8   = Int8 
    | x == vg_PATH_DATATYPE_S_16  = Int16 
    | x == vg_PATH_DATATYPE_S_32  = Int32
    | x == vg_PATH_DATATYPE_F     = Float     
    | otherwise = error ("unmarshalPathDatatype: illegal value " ++ show x)


marshalPathSegment :: PathSegment -> VGenum
marshalPathSegment x = case x of
    ClosePath -> vg_CLOSE_PATH
    MoveTo    -> vg_MOVE_TO
    LineTo    -> vg_LINE_TO
    HLineTo   -> vg_HLINE_TO
    VLineTo   -> vg_VLINE_TO
    QuadTo    -> vg_QUAD_TO
    CubicTo   -> vg_CUBIC_TO
    SQuadTo   -> vg_SQUAD_TO
    SCubicTo  -> vg_SCUBIC_TO
    SCCWArcTo -> vg_SCCWARC_TO
    SCWArcTo  -> vg_SCWARC_TO
    LCCWArcTo -> vg_LCCWARC_TO
    LCWArcTo  -> vg_LCWARC_TO
 


marshalPathCommand :: PathCommand -> VGenum
marshalPathCommand x = case x of
    MoveToAbs    -> vg_MOVE_TO_ABS
    MoveToRel    -> vg_MOVE_TO_REL
    LineToAbs    -> vg_LINE_TO_ABS
    LineToRel    -> vg_LINE_TO_REL
    HLineToAbs   -> vg_HLINE_TO_ABS
    HLineToRel   -> vg_HLINE_TO_REL
    VLineToAbs   -> vg_VLINE_TO_ABS
    VLineToRel   -> vg_VLINE_TO_REL
    QuadToAbs    -> vg_QUAD_TO_ABS
    QuadToRel    -> vg_QUAD_TO_REL
    CubicToAbs   -> vg_CUBIC_TO_ABS
    CubicToRel   -> vg_CUBIC_TO_REL
    SQuadToAbs   -> vg_SQUAD_TO_ABS
    SQuadToRel   -> vg_SQUAD_TO_REL
    SCubicToAbs  -> vg_SCUBIC_TO_ABS
    SCubicToRel  -> vg_SCUBIC_TO_REL
    SCCWArcToAbs -> vg_SCCWARC_TO_ABS
    SCCWArcToRel -> vg_SCCWARC_TO_REL
    SCWArcToAbs  -> vg_SCWARC_TO_ABS
    SCWArcToRel  -> vg_SCWARC_TO_REL
    LCCWArcToAbs -> vg_LCCWARC_TO_ABS
    LCCWArcToRel -> vg_LCCWARC_TO_REL
    LCWArcToAbs  -> vg_LCWARC_TO_ABS
    LCWArcToRel  -> vg_LCWARC_TO_REL

      
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
    | otherwise                                       = error $ 
          "unmarshalPathCapabilities: illegal value " ++ show x
        
 
marshalCapStyle :: CapStyle -> VGenum
marshalCapStyle x = case x of
    CButt   -> vg_CAP_BUTT
    CRound  -> vg_CAP_ROUND
    CSquare -> vg_CAP_SQUARE

    
marshalJoinStyle :: JoinStyle -> VGenum
marshalJoinStyle x = case x of
    JMiter -> vg_JOIN_MITER
    JRound -> vg_JOIN_ROUND 
    JBevel -> vg_JOIN_BEVEL


marshalFillRule :: FillRule -> VGenum
marshalFillRule x = case x of 
    EvenOdd -> vg_EVEN_ODD
    NonZero -> vg_NON_ZERO

marshalPaintMode :: PaintMode -> VGenum     
marshalPaintMode x = case x of 
    StrokePath -> vg_STROKE_PATH
    FillPath   -> vg_FILL_PATH

{-
    
unmarshalPaintMode :: VGenum -> PaintMode 
unmarshalPaintMode x
    | x == vg_STROKE_PATH         = StrokePath 
    | x == vg_FILL_PATH           = FillPath 
    | otherwise = error ("unmarshalPaintMode: illegal value " ++ show x)
-}

