{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenVG.VG.Scissoring
-- Copyright   :  (c) Stephen Tetley 2008, 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  unstable
-- Portability :  GHC
--
-- This module corresponds to section 7 (Scissoring, Masking and Clearing) 
-- of the OpenVG 1.0.1 specs.
--
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenVG.VG.Scissoring (
  -- * Scissoring
  scissoring, 
  ScissorRect, 
  maxScissorRects,
  scissorRects,
  
  -- * Alpha masking
  MaskOperation(..),
  alphaMasking,
  
  -- * Fast clearing
  clearColor,
  clear  
) where

import Graphics.Rendering.OpenVG.VG.BasicTypes ( 
    VGint, VGfloat, marshalBool, unSize )
import Graphics.Rendering.OpenVG.VG.CFunDecls ( vgClear )
import Graphics.Rendering.OpenVG.VG.Parameters ( 
    ParamType ( Scissoring, ScissorRects, MaxScissorRects, 
                Masking, ClearColor ),   
    seti, geti, setiv, setfv )

import Graphics.Rendering.OpenGL.GL.CoordTrans ( Position(..), Size(..) )
import Graphics.Rendering.OpenGL.GL.VertexSpec ( Color4(..) )

import Data.StateVar (
    SettableStateVar, makeSettableStateVar,
    GettableStateVar, makeGettableStateVar ) 


--------------------------------------------------------------------------------
-- Scissoring

-- | Enable or disable scissoring.
scissoring :: SettableStateVar Bool  
scissoring = makeSettableStateVar $ \a -> 
    seti Scissoring (fromIntegral $ marshalBool a) 

type ScissorRect = (Position, Size) 
    
-- | Get the maximum number of scissoring rectangles.
maxScissorRects :: GettableStateVar VGint
maxScissorRects = makeGettableStateVar $ geti MaxScissorRects


-- | Specify the scissoring rectangles.
scissorRects :: SettableStateVar [ScissorRect]
scissorRects = makeSettableStateVar $ \ss ->
    setiv ScissorRects (foldr f [] ss) where 
        f ((Position mx my), sz) a = let (w,h) = unSize sz in mx:my:w:h:a 

--------------------------------------------------------------------------------
-- Alpha masking

-- | @MaskOperation@ corresponds to the OpenVG enumeration @VGMaskOperation@.    
data MaskOperation =
     ClearMask
   | FillMask
   | SetMask
   | UnionMask
   | IntersectMask
   | SubtractMask
   deriving ( Eq, Ord, Show )

-- | Enable or disable alpha masking.   
alphaMasking :: SettableStateVar Bool
alphaMasking = makeSettableStateVar $ \a -> seti Masking (marshalBool a)  

-- vgMask not implemented in shiva-vg

--------------------------------------------------------------------------------
-- Fast clearing

-- | Set the color for clearing.
clearColor :: SettableStateVar (Color4 VGfloat)
clearColor = makeSettableStateVar $ 
    \(Color4 r g b a) -> setfv ClearColor [r,g,b,a]

-- | @clear@ corresponds to the OpenVG function @vgClear@.
clear :: Position -> Size -> IO ()
clear (Position x y) sz = let (w,h) = unSize sz in vgClear x y w h


--------------------------------------------------------------------------------

{-
-- Defined but not (YET) used: marshalMaskOperation
marshalMaskOperation :: MaskOperation -> VGenum
marshalMaskOperation x = case x of
    ClearMask -> vg_CLEAR_MASK
    FillMask -> vg_FILL_MASK
    SetMask -> vg_SET_MASK
    UnionMask -> vg_UNION_MASK
    IntersectMask -> vg_INTERSECT_MASK
    SubtractMask -> vg_SUBTRACT_MASK
-}


