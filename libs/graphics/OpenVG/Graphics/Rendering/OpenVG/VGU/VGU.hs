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

import Graphics.Rendering.OpenVG.VGU.CInternals
import Graphics.Rendering.OpenVG.VG.BasicTypes ( 
    VGfloat, VGenum, VGPath, marshalBool )

import Foreign.Marshal.Array ( newArray )
    
data VGU_ErrorCode = 
     VGU_NoError
   | VGU_BadHandleError
   | VGU_IllegalArgumentError
   | VGU_OutOfMemoryError
   | VGU_PathCapabilityError
   | VGU_BadWarpError
   deriving ( Eq, Ord, Show )


withErrorCode :: IO VGUErrorCode' -> IO VGU_ErrorCode
withErrorCode f = do 
  a <- f
  return $ unmarshalVGU_ErrorCode a
    
line :: VGPath -> VGfloat -> VGfloat -> VGfloat -> VGfloat -> IO VGU_ErrorCode
line path x0 y0 x1 y1 = withErrorCode $ vguLine path x0 y0 x1 y1 

    
polygon :: VGPath -> [VGfloat] -> Bool -> IO VGU_ErrorCode
polygon path pts closed = do
    pts' <- newArray pts 
    withErrorCode $ vguPolygon path pts' (fromIntegral $ length pts) 
                                         (marshalBool closed)
            
rect :: VGPath -> VGfloat -> VGfloat -> VGfloat -> VGfloat
               -> IO VGU_ErrorCode
rect path x y w h = withErrorCode $ vguRect path x y w h

roundRect :: VGPath -> VGfloat -> VGfloat -> VGfloat -> VGfloat
                    -> VGfloat -> VGfloat 
                    -> IO VGU_ErrorCode
roundRect path x y w h aw ah = withErrorCode $ vguRoundRect path x y w h aw ah

ellipse :: VGPath -> VGfloat -> VGfloat -> VGfloat -> VGfloat 
                  -> IO VGU_ErrorCode  
ellipse path cx cy w h = withErrorCode $ vguEllipse path cx cy w h


data ArcType =
    ArcOpen
  | ArcChord
  | ArcPie
   deriving ( Eq, Ord, Show )

arc :: VGPath -> VGfloat -> VGfloat -> VGfloat -> VGfloat
           -> VGfloat -> VGfloat -> ArcType 
           -> IO VGU_ErrorCode  
arc path x y w h sa ae atyp = 
    withErrorCode $ vguArc path x y w h sa ae (marshalArcType atyp)
           
--------------------------------------------------------------------------------
   
unmarshalVGU_ErrorCode :: VGenum -> VGU_ErrorCode
unmarshalVGU_ErrorCode x
    | x == vgu_NO_ERROR                 = VGU_NoError
    | x == vgu_BAD_HANDLE_ERROR         = VGU_BadHandleError
    | x == vgu_ILLEGAL_ARGUMENT_ERROR   = VGU_IllegalArgumentError
    | x == vgu_OUT_OF_MEMORY_ERROR      = VGU_OutOfMemoryError
    | x == vgu_PATH_CAPABILITY_ERROR    = VGU_PathCapabilityError
    | x == vgu_BAD_WARP_ERROR           = VGU_BadWarpError
    | otherwise = error ("unmarshalVGU_ErrorCode: illegal value " ++ show x)

marshalArcType :: ArcType -> VGenum
marshalArcType x = case x of 
    ArcOpen -> vgu_ARC_OPEN
    ArcChord -> vgu_ARC_CHORD
    ArcPie -> vgu_ARC_PIE
                    