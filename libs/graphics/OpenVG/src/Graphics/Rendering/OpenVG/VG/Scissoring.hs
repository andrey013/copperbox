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
  mask,
  
  -- * Fast clearing
  clearColor,
  clear  

) where


import Graphics.Rendering.OpenVG.VG.Parameters
import Graphics.Rendering.OpenVG.VG.Utils ( marshalBool, unSize, unSizeM )

import Graphics.Rendering.OpenVG.Raw.VG.Core101 ( 
    VGint, VGfloat, VGenum, VGImage )
import Graphics.Rendering.OpenVG.Raw.VG.Scissoring

import Graphics.Rendering.OpenGL.GL.CoordTrans ( Position(..), Size(..) )
import Graphics.Rendering.OpenGL.GL.VertexSpec ( Color4(..) )

import Data.StateVar (
    SettableStateVar, makeSettableStateVar,
    GettableStateVar, makeGettableStateVar ) 


--------------------------------------------------------------------------------
-- Scissoring

-- | Enable or disable scissoring.
--
-- 'scissoring' is a write-only state variable corresponding to
-- @VG_SCISSORING@.
--
scissoring :: SettableStateVar Bool  
scissoring = makeSettableStateVar $ 
    seti Scissoring . fromIntegral . marshalBool

type ScissorRect = (Position, Size) 
    
-- | Get the maximum number of scissoring rectangles.
--
-- 'maxScissorRects' is a read-only state variable corresponding 
-- to @VG_MAX_SCISSOR_RECTS@ 
--
maxScissorRects :: GettableStateVar VGint
maxScissorRects = makeGettableStateVar $ geti MaxScissorRects


-- | Specify the scissoring rectangles.
--
-- 'scissorRects' is a write-only state variable corresponding to 
-- @VG_SCISSOR_RECTS@.
--
scissorRects :: SettableStateVar [ScissorRect]
scissorRects = makeSettableStateVar $ \ss ->
    setiv ScissorRects (foldr f [] ss) where 
        f ((Position mx my), sz) a = let (w,h) = unSize sz in mx:my:w:h:a 

--------------------------------------------------------------------------------
-- Alpha masking

-- | 'MaskOperation' enumerations the possible mask operations.    
data MaskOperation =
     ClearMask
   | FillMask
   | SetMask
   | UnionMask
   | IntersectMask
   | SubtractMask
   deriving ( Eq, Ord, Show )

-- | Enable or disable alpha masking.   
--
-- 'alphaMasking' is a write-only state variable.
--
alphaMasking :: SettableStateVar Bool
alphaMasking = makeSettableStateVar $ seti Masking . marshalBool

-- | Modify the alpha mask values according to the supplied
-- 'MaskOperation'.
--
mask :: VGImage -> MaskOperation -> Position -> Size -> IO ()
mask img mop (Position x y) = 
    unSizeM $ vgMask img (marshalMaskOperation mop) x y

--------------------------------------------------------------------------------
-- Fast clearing

-- | Set the color for clearing.
--
-- 'clearColor' is a write-only state variable corresponding to
-- @VG_CLEAR_COLOR@
--
clearColor :: SettableStateVar (Color4 VGfloat)
clearColor = makeSettableStateVar $ 
    \(Color4 r g b a) -> setfv ClearColor [r,g,b,a]

-- | Fill the specified portion of the drawing surface with the
-- current clear color.
-- 
-- 'clear' corresponds to the OpenVG function @vgClear@.
--
clear :: Position -> Size -> IO ()
clear (Position x y) = unSizeM $ vgClear x y


--------------------------------------------------------------------------------


marshalMaskOperation :: MaskOperation -> VGenum
marshalMaskOperation x = case x of
    ClearMask     -> vg_CLEAR_MASK
    FillMask      -> vg_FILL_MASK
    SetMask       -> vg_SET_MASK
    UnionMask     -> vg_UNION_MASK
    IntersectMask -> vg_INTERSECT_MASK
    SubtractMask  -> vg_SUBTRACT_MASK



