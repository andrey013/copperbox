{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenVG.VGU.VGU
-- Copyright   :  (c) Stephen Tetley 2008-2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  unstable
-- Portability :  GHC
--
-- This module corresponds to section 16 (The VGU Utility Library) 
-- of the OpenVG 1.0.1 specs.
--
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenVG.VGU.VGU (
  line,
  polygon,
  rect,
  roundRect,
  ellipse,
  ArcType(..),
  arc
) where

import Graphics.Rendering.OpenVG.VG.Utils ( marshalBool )

import Graphics.Rendering.OpenVG.Raw.VG.Core101 (
    VGenum, VGfloat, VGPath )

import Graphics.Rendering.OpenVG.Raw.VGU.VGU

import Graphics.Rendering.OpenVG.VGU.ErrorsInternal ( 
        VGU_ErrorCode, withErrorCode )


import Foreign.Marshal.Array ( newArray )

-- | Append a line segment to a path.
--
line :: VGPath -> VGfloat -> VGfloat -> VGfloat -> VGfloat -> IO VGU_ErrorCode
line path x0 y0 x1 y1 = withErrorCode $ vguLine path x0 y0 x1 y1 

    
-- | Append a polyline or polygon to a path.
--
polygon :: VGPath -> [VGfloat] -> Bool -> IO VGU_ErrorCode
polygon path pts closed = do
    pts' <- newArray pts 
    withErrorCode $ vguPolygon path pts' (fromIntegral $ length pts) 
                                         (marshalBool closed)
           
-- | Append a rectangle with its lower left corner at @(x,y)@ 
-- to a path.
--
rect :: VGPath -> VGfloat -> VGfloat -> VGfloat -> VGfloat -> IO VGU_ErrorCode
rect path x y w h = withErrorCode $ vguRect path x y w h


-- | Append a rounded corner rectangle with its lower left 
-- corner at @(x,y)@ to a path.
--
roundRect :: VGPath -> VGfloat -> VGfloat -> VGfloat -> VGfloat
          -> VGfloat -> VGfloat 
          -> IO VGU_ErrorCode
roundRect path x y w h aw ah = withErrorCode $ vguRoundRect path x y w h aw ah


-- | Append an ellipse to the path.
--
ellipse :: VGPath -> VGfloat -> VGfloat -> VGfloat -> VGfloat 
        -> IO VGU_ErrorCode  
ellipse path cx cy w h = withErrorCode $ vguEllipse path cx cy w h


-- | 'ArcType' enumerates the control styles of an arc.
--
data ArcType =
    ArcOpen
  | ArcChord
  | ArcPie
   deriving ( Eq, Ord, Show )

-- | Append an elliptical arc to a path.
--
arc :: VGPath  -> VGfloat -> VGfloat -> VGfloat -> VGfloat
    -> VGfloat -> VGfloat -> ArcType 
    -> IO VGU_ErrorCode 
arc path x y w h sa ae atyp = 
    withErrorCode $ vguArc path x y w h sa ae (marshalArcType atyp)
           
--------------------------------------------------------------------------------

marshalArcType :: ArcType -> VGenum
marshalArcType x = case x of 
    ArcOpen  -> vgu_ARC_OPEN
    ArcChord -> vgu_ARC_CHORD
    ArcPie   -> vgu_ARC_PIE

-- 


