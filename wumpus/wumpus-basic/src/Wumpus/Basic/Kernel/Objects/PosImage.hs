{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Kernel.Objects.PosImage
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  highly unstable
-- Portability :  GHC 
--
-- Extended Graphic object - a rectangular /positionable/ Image.
-- 
-- This graphic object has a more flexible API for positioning 
-- than other graphic objects. Rather than a LocGraphic which 
-- supports a single method of positioning at some start-point,
-- a @PosGraphic@ can be drawn at its center or locations on its 
-- outer rectangle.
--
--------------------------------------------------------------------------------

module Wumpus.Basic.Kernel.Objects.PosImage
  (

    RectPosition(..)
  , ObjectPos(..)

  -- * Positionable image

  , PosImage
  , DPosImage 

  , PosGraphic
  , DPosGraphic

  , PosThetaImage
  , DPosThetaImage

  , PosThetaGraphic
  , DPosThetaGraphic

  , makePosImage
  , makePosThetaImage

  , startPos
  , atStartPos

  , objectPosBounds

  ) where


import Wumpus.Basic.Kernel.Base.BaseDefs
import Wumpus.Basic.Kernel.Base.ContextFun
import Wumpus.Basic.Kernel.Objects.BaseObjects
import Wumpus.Basic.Kernel.Objects.Displacement

import Wumpus.Core                              -- package: wumpus-core

import Data.AffineSpace                         -- package: vector-space

-- | Datatype enumerating positions within a rectangle that can be
-- derived for a 'PosGraphic'.  
--
data RectPosition = CENTER | NN | SS | EE | WW | NE | NW | SE | SW 
  deriving (Enum,Eq,Ord,Show)


-- | Utility datatype representing orientation within a 
-- rectangular /frame/. ObjectPos is useful for graphics such as 
-- text where the start point is not necessarily at the center 
-- (or bottom left).
--
-- > x_minor is the horizontal distance from the left to the start point
-- >
-- > x_major is the horizontal distance from the start point to the right
-- >
-- > y_minor is the vertical distance from the bottom to the start point
-- >
-- > y_major is the vertical distance from the start point to the top
--
-- Values should be not be negative!
--
-- 
data ObjectPos u = ObjectPos 
      { op_x_minor      :: !u
      , op_x_major      :: !u
      , op_y_minor      :: !u
      , op_y_major      :: !u
      }
  deriving (Eq,Ord,Show)



type instance DUnit (ObjectPos u)   = u



-- | A positionable Image.
--
type PosImage u a = CF2  (Point2 u) RectPosition (ImageAns u a)
    
-- | Version of PosImage specialized to Double for the unit type.
--
type DPosImage a = PosImage Double a



-- | A positionable Graphic.
--
type PosGraphic u = PosImage u (UNil u) 
    
-- | Version of PosGraphic specialized to Double for the unit type.
--
type DPosGraphic = PosGraphic Double



-- | A positionable Image that supports drawing at some angle of 
-- inclination.
-- 
-- Note - the rectangle frame is expected to represent an 
-- orthogonal frame bounding the maximum hull of the Image, the 
-- frame is not intended to be inclined itself.
--
type PosThetaImage u a = CF3  (Point2 u) RectPosition Radian (ImageAns u a)
    
-- | Version of PosThetaImage specialized to Double for the unit type.
--
type DPosThetaImage a = PosThetaImage Double a



-- | A positionable Graphic that supports drawing at some angle of
-- inclination.
--
-- Note - the rectangle frame is expected to represent an 
-- orthogonal frame bounding the maximum hull of the Image, the 
-- frame is not intended to be inclined itself.
--
type PosThetaGraphic u = PosThetaImage u (UNil u) 
    
-- | Version of PosThetaGraphic specialized to Double for the unit type.
--
type DPosThetaGraphic = PosThetaGraphic Double





--------------------------------------------------------------------------------


instance (Fractional u, Ord u) => OPlus (ObjectPos u) where
  oplus = concatObjectPos


-- | Concatenation here essentially turns both ObjectPos objects
-- into /center-form/ then finds the maximum rectangle.
--
concatObjectPos :: (Fractional u, Ord u) 
                => ObjectPos u -> ObjectPos u -> ObjectPos u
concatObjectPos op0 op1 = ObjectPos hw hw hh hh
  where
    (hw0,hh0) = halfDists op0
    (hw1,hh1) = halfDists op1
    hw        = max hw0 hw1
    hh        = max hh0 hh1



--------------------------------------------------------------------------------


-- | Find the half-width and half-height of an ObjectPos.
-- 
-- Essentially this is /center-form/ of an ObjectPos, but 
-- in /center-form/ there is duplication: 
--
-- > xminor == xmajor
-- > yminor == ymajor
-- 
-- So instead, the result type is just a pair.
--
halfDists :: Fractional u => ObjectPos u -> (u,u)
halfDists (ObjectPos xmin xmaj ymin ymaj) = 
    (0.5 * (xmin+xmaj), 0.5 * (ymin+ymaj))


-- | 'makePosImage' : @ object_pos * loc_graphic -> PosGraphic @ 
--
-- Create a 'PosImage' from an 'ObjectPos' describing how it
-- is orientated within a border rectangle and a 'LocImage' that 
-- draws it.
--
makePosImage :: Fractional u 
             => ObjectPos u -> LocImage u a -> PosImage u a
makePosImage opos gf = promoteR2 $ \start rpos -> 
    let v1 = startVector rpos opos in gf `at` displaceVec v1 start



-- | 'makePosImage' : @ object_pos * loc_graphic -> PosGraphic @ 
--
-- Create a 'PosThetaImage' from an 'ObjectPos' describing how it
-- is orientated within a border rectangle and a 'LocThetaImage' 
-- that draws it at some angle of inclination.
--
makePosThetaImage :: Fractional u 
             => ObjectPos u -> LocThetaImage u a -> PosThetaImage u a
makePosThetaImage opos gf = promoteR3 $ \start rpos theta -> 
    let v1 = startVector rpos opos 
    in atRot gf (displaceVec v1 start) theta



infixr 1 `startPos`

-- | 'startPos' : @ pos_image * rect_pos -> LocImage @
--
-- /Downcast/ a 'PosImage' to a 'LocImage' by supplying it 
-- with a 'RectPosition' (start position).
--  
startPos :: Floating u 
         => PosImage u a -> RectPosition -> LocImage u a
startPos = apply1R2
 
-- | 'atStartPos' : @ pos_image * start_point * rect_pos -> LocImage @
--
-- /Downcast/ a 'PosGraphic' to an 'Image' by supplying it 
-- with an initial point and a 'RectPosition' (start position).
--  
atStartPos ::  Floating u 
           => PosImage u a -> Point2 u -> RectPosition -> Image u a
atStartPos = apply2R2

-- | The vector from some Rectangle position to the start point.
--
startVector :: Fractional u => RectPosition -> ObjectPos u -> Vec2 u
startVector rpos (ObjectPos xminor xmajor yminor ymajor) = go rpos
  where
    w         = xminor + xmajor
    h         = yminor + ymajor
    hw        = 0.5 * w
    hh        = 0.5 * h
    
    -- CENTER, NN, SS, EE, WW all go to bottomleft then add back 
    -- the minors.

    go CENTER = V2 ((-hw) + xminor) ((-hh) + yminor)
    go NN     = V2 ((-hw) + xminor) ((-h)  + yminor)
    go SS     = V2 ((-hw) + xminor)  yminor
    go EE     = V2 ((-w)  + xminor) ((-hh) + yminor)
    go WW     = V2 xminor           ((-hh) + yminor)
    go NE     = V2 (-xmajor)        (-ymajor)
    go SE     = V2 (-xmajor)          yminor
    go SW     = V2 xminor           yminor
    go NW     = V2 xminor           (-ymajor)



-- | Calculate the bounding box formed by locating the 'ObjectPos'
-- at the supplied point.
-- 
objectPosBounds :: Fractional u 
                => Point2 u -> RectPosition -> ObjectPos u -> BoundingBox u
objectPosBounds (P2 x y) pos (ObjectPos xmin xmaj ymin ymaj) = go pos
  where
    w         = xmin + xmaj
    h         = ymin + ymaj
    hw        = 0.5 * w
    hh        = 0.5 * h
    bbox      = \bl -> BBox bl (bl .+^ vec w h)

    go CENTER = bbox $ P2 (x-hw) (y-hh)
    go NN     = bbox $ P2 (x-hw) (y-h)
    go SS     = bbox $ P2 (x-hw)  y
    go EE     = bbox $ P2 (x-w)  (y-hh)
    go WW     = bbox $ P2  x     (y-hh)
    go NE     = bbox $ P2 (x-w)  (y-h)
    go SE     = bbox $ P2 (x-w)   y
    go SW     = bbox $ P2 x       y
    go NW     = bbox $ P2 x      (y-h)


