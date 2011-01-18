{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Kernel.Objects.PosImage
-- Copyright   :  (c) Stephen Tetley 2010
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
-- a @PosGraphic@ can be drawn at its corners or other locations
-- on its outer rectangle.
--
-- PosGraphic is anticipated to be most applicable to text 
-- objects where positioning at /baseline-left/ is not always
-- satifactory.
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

  , makePosImage

  , setPosition

  ) where


import Wumpus.Basic.Kernel.Base.ContextFun
import Wumpus.Basic.Kernel.Objects.BaseObjects
import Wumpus.Basic.Kernel.Objects.Displacement

import Wumpus.Core                              -- package: wumpus-core


-- | Datatype enumerating positions within a rectangle that can be
-- derived for a 'PosImage'.  
--
-- Note - for some implementations of PosImage, certain positions 
-- may coincide. For instance, @SE@ and @BL_LEFT@ will coincide 
-- for a PosImage with no x- and y-minor components.
--
data RectPosition = CENTER | NN | SS | EE | WW | NE | NW | SE | SW 
                  | BL_LEFT | BL_CENTER | BL_RIGHT
  deriving (Enum,Eq,Ord,Show)


-- | Datatype representing positioning within a rectangular /frame/.
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
data ObjectPos u = ObjectPos 
      { op_x_minor      :: !u
      , op_x_major      :: !u
      , op_y_minor      :: !u
      , op_y_major      :: !u
      }
  deriving (Eq,Ord,Show)

-- | A positionable image.
--
data PosImage u a = PosImage
      { pos_position    :: ObjectPos u
      , pos_image       :: LocThetaImage u a
      }
       
-- | Version of PosImage specialed to Double for the unit type.
--
type DPosImage a = PosImage Double a


-- | A positionable Graphic.
--
type PosGraphic u = PosImage u (UNil u)

-- | Version of PosGraphic specialed to Double for the unit type.
--
type DPosGraphic  = PosGraphic Double


-- | Create a 'PosImage'.
--
makePosImage :: ObjectPos u -> LocThetaImage u a -> PosImage u a
makePosImage opos img = PosImage { pos_position    = opos
                                 , pos_image       = img  }

type instance DUnit (ObjectPos u) = u
type instance DUnit (PosImage u a) = u



-- | 'setPosition' : @ start_pos * pos_image -> LocThetaImage @
--
-- /Downcast/ a 'PosImage' to a 'LocThetaImage' by supplying it 
-- with a 'RectPosition'.
--  
setPosition :: Floating u => RectPosition -> PosImage u a -> LocThetaImage u a
setPosition rp (PosImage opos mf) = promoteR2 $ \pt theta ->
    let (V2 dx dy) = displacement opos rp 
        moveF      = displaceParallel (-dx) theta . displacePerpendicular (-dy) theta
    in atRot mf (moveF pt) theta



displacement :: Fractional u => ObjectPos u -> RectPosition -> Vec2 u
displacement (ObjectPos xminor xmajor yminor ymajor) pos = go pos
  where
    width       = xminor + xmajor
    height      = yminor + ymajor

    go CENTER = V2 ((0.5 * width) - xminor) ((0.5 * height) - yminor)
    go NN     = V2 ((0.5 * width) - xminor)  ymajor
    go SS     = V2 ((0.5 * width) - xminor)  (-yminor)
    go EE     = V2 xmajor    ((0.5 * height) - yminor)
    go WW     = V2 (-xminor) ((0.5 * height) - yminor)
    go NE     = V2   xmajor    ymajor
    go SE     = V2   xmajor  (-yminor)
    go SW     = V2 (-xminor) (-yminor)
    go NW     = V2 (-xminor)   ymajor
    go BL_LEFT    = V2 (-xminor)  0
    go BL_CENTER  = V2 ((0.5 * width) - xminor) 0
    go BL_RIGHT   = V2   xmajor   0  

