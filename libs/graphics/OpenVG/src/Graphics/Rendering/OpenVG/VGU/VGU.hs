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


import Graphics.Rendering.OpenVG.VG.BasicTypes ( 
    VGfloat, VGenum, VGPath, marshalBool )
import Graphics.Rendering.OpenVG.VGU.CInternals
import Graphics.Rendering.OpenVG.VGU.ErrorsInternal ( recordErrorCode )
import Foreign.Marshal.Array ( newArray )

    
line :: VGPath -> VGfloat -> VGfloat -> VGfloat -> VGfloat -> IO ()
line path x0 y0 x1 y1 = withErrorCode $ vguLine path x0 y0 x1 y1 

    
polygon :: VGPath -> [VGfloat] -> Bool -> IO ()
polygon path pts closed = do
    pts' <- newArray pts 
    withErrorCode $ vguPolygon path pts' (fromIntegral $ length pts) 
                                         (marshalBool closed)
            
rect :: VGPath -> VGfloat -> VGfloat -> VGfloat -> VGfloat
               -> IO ()
rect path x y w h = withErrorCode $ vguRect path x y w h

roundRect :: VGPath -> VGfloat -> VGfloat -> VGfloat -> VGfloat
                    -> VGfloat -> VGfloat 
                    -> IO ()
roundRect path x y w h aw ah = withErrorCode $ vguRoundRect path x y w h aw ah

ellipse :: VGPath -> VGfloat -> VGfloat -> VGfloat -> VGfloat 
                  -> IO ()  
ellipse path cx cy w h = withErrorCode $ vguEllipse path cx cy w h


data ArcType =
    ArcOpen
  | ArcChord
  | ArcPie
   deriving ( Eq, Ord, Show )

arc :: VGPath -> VGfloat -> VGfloat -> VGfloat -> VGfloat
           -> VGfloat -> VGfloat -> ArcType 
           -> IO ()  
arc path x y w h sa ae atyp = 
    withErrorCode $ vguArc path x y w h sa ae (marshalArcType atyp)
           
--------------------------------------------------------------------------------

marshalArcType :: ArcType -> VGenum
marshalArcType x = case x of 
    ArcOpen -> vgu_ARC_OPEN
    ArcChord -> vgu_ARC_CHORD
    ArcPie -> vgu_ARC_PIE

-- 
withErrorCode :: IO VGenum -> IO ()
withErrorCode f = do 
  a <- f
  recordErrorCode a
  return ()
                      