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
-- of the OpenVG 1.0.1 specs.
--
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenVG.VG.Scissoring (
  MaskOperation(..),
  scissoring, 
  maxScissorRects,
  ScissorRect, scissorRects,
  alphaMasking,
  clear
  
) where

import Graphics.Rendering.OpenVG.VG.BasicTypes ( 
    VGenum, VGint, VGfloat, marshalBool )
import Graphics.Rendering.OpenVG.VG.CFunDecls ( vgClear )
import Graphics.Rendering.OpenVG.VG.Constants (
    vg_CLEAR_MASK, vg_FILL_MASK, vg_SET_MASK, vg_UNION_MASK, 
    vg_INTERSECT_MASK, vg_SUBTRACT_MASK )  
import Graphics.Rendering.OpenVG.VG.Parameters ( 
    ParamType ( ParamScissoring, ParamScissorRects, ParamMaxScissorRects, 
                ParamMasking, ParamClearColor ),   
    seti, geti, setiv, setfv )
import Graphics.Rendering.OpenGL.GL.StateVar (
    SettableStateVar, makeSettableStateVar,
    GettableStateVar, makeGettableStateVar ) 
import Graphics.Rendering.OpenGL.GL.VertexSpec ( Color4(..) )
            
data MaskOperation =
     ClearMask
   | FillMask
   | SetMask
   | UnionMask
   | IntersectMask
   | SubtractMask
   deriving ( Eq, Ord, Show )

scissoring :: SettableStateVar Bool  
scissoring = makeSettableStateVar $ \a -> 
    seti ParamScissoring (fromIntegral $ marshalBool a) 

maxScissorRects :: GettableStateVar VGint
maxScissorRects = makeGettableStateVar $ geti ParamMaxScissorRects

type ScissorRect = (VGint,VGint,VGint,VGint)

-- | ScissorRects are discarded immediately and cannot be retrieved with a getvi
scissorRects :: SettableStateVar [ScissorRect]
scissorRects = makeSettableStateVar $ \ss ->
    setiv ParamScissorRects (foldr f [] ss) where 
        f (mx,my,w,h) a = mx:my:w:h:a 

alphaMasking :: SettableStateVar Bool
alphaMasking = makeSettableStateVar $ \a -> 
    seti ParamMasking (marshalBool a)  

-- vgMask not implemented in shiva-vg


clearColor :: Color4 VGfloat -> IO ()
clearColor (Color4 r g b a) = setfv ParamClearColor [r,g,b,a]

clear :: VGint -> VGint -> VGint -> VGint -> IO ()
clear x y w h = vgClear x y w h


--------------------------------------------------------------------------------

marshalMaskOperation :: MaskOperation -> VGenum
marshalMaskOperation x = case x of
    ClearMask -> vg_CLEAR_MASK
    FillMask -> vg_FILL_MASK
    SetMask -> vg_SET_MASK
    UnionMask -> vg_UNION_MASK
    IntersectMask -> vg_INTERSECT_MASK
    SubtractMask -> vg_SUBTRACT_MASK
