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

  , runPosImage
  , rawPosImage
  , startPos
  , atStartPos

  , promote_pi1
  , promote_pi2
  , lift_pi1
  , lift_pi2

  , posImage

  , objectPosBounds

  ) where


import Wumpus.Basic.Kernel.Base.BaseDefs
import Wumpus.Basic.Kernel.Base.DrawingContext
import Wumpus.Basic.Kernel.Base.WrappedPrimitive
import Wumpus.Basic.Kernel.Objects.Displacement
import Wumpus.Basic.Kernel.Objects.Image
import Wumpus.Basic.Kernel.Objects.ImageBasis
import Wumpus.Basic.Kernel.Objects.LocImage

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






-- | A positionable Image.
--
newtype PosImage t u = PosImage { 
          getPosImage :: DrawingContext -> Point2 u -> 
                         RectPosition   -> (t u, CatPrim) }
    
-- | Version of PosImage specialized to Double for the unit type.
--
type DPosImage t = PosImage t Double




-- | A positionable Graphic.
--
type PosGraphic u = PosImage UNil u
    
-- | Version of PosGraphic specialized to Double for the unit type.
--
type DPosGraphic = PosGraphic Double





instance MoveStart PosImage where
  moveStart fn gf = PosImage $ \ctx pt rpos -> getPosImage gf ctx (fn pt) rpos


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



--------------------------------------------------------------------------------
-- builders and destructors


runPosImage :: PosImage t u -> DrawingContext -> Point2 u -> RectPosition 
            -> (t u, CatPrim)
runPosImage gf ctx pt rpos = getPosImage gf ctx pt rpos

-- This seems to be the one for down casting...
-- 
rawPosImage :: (DrawingContext -> Point2 u -> RectPosition -> (t u, CatPrim)) 
            -> PosImage t u
rawPosImage fn = PosImage $ \ctx pt rpos -> fn ctx pt rpos



infixr 1 `startPos`

-- | 'startPos' : @ pos_image * rect_pos -> LocImage @
--
-- /Downcast/ a 'PosImage' to a 'LocImage' by supplying it 
-- with a 'RectPosition' (start position).
--  
startPos :: Floating u 
         => PosImage t u -> RectPosition -> LocImage t u
startPos gf rpos = rawLocImage (\ctx pt -> getPosImage gf ctx pt rpos) 
 



-- | 'atStartPos' : @ pos_image * start_point * rect_pos -> LocImage @
--
-- /Downcast/ a 'PosGraphic' to an 'Image' by supplying it 
-- with an initial point and a 'RectPosition' (start position).
--  
atStartPos ::  Floating u 
           => PosImage t u -> Point2 u -> RectPosition -> Image t u
atStartPos gf pt rpos = rawImage (\ctx -> getPosImage gf ctx pt rpos) 

--
-- Design note
--
-- Do we need an @intoPosImage@ function? 
-- It would mean adding @PosQuery@ and all its details...
--


promote_pi1 :: (RectPosition -> LocImage t u) -> PosImage t u
promote_pi1 gf = 
    PosImage $ \ctx pt rpos -> runLocImage (gf rpos) ctx pt


promote_pi2 :: (Point2 u -> RectPosition -> Image t u) -> PosImage t u
promote_pi2 gf = 
    PosImage $ \ctx pt rpos -> runImage (gf pt rpos) ctx


lift_pi1 :: LocImage t u -> PosImage t u
lift_pi1 gf = PosImage $ \ctx pt _ -> runLocImage gf ctx pt


lift_pi2 :: Image t u -> PosImage t u
lift_pi2 gf = PosImage $ \ctx _ _ -> runImage gf ctx 


-- | 'posImage' : @ object_pos * loc_graphic -> PosGraphic @ 
--
-- Create a 'PosImage' from an 'ObjectPos' describing how it
-- is orientated within a border rectangle and a 'LocImage' that 
-- draws it.
--
-- This is the /primary/ constructor for PosImages. Because the
-- PosImage type is considered as a specialized object it does
-- not have the range of functions of LocImage or LocThetaImage.
-- 
posImage :: Fractional u 
         => ObjectPos u -> LocImage t u -> PosImage t u
posImage opos gf = promote_pi2 $ \start rpos -> 
    let v1 = startVector rpos opos in gf `at` displaceVec v1 start




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


