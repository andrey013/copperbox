{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Kernel.Objects.PosGraphic
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
-- PosGraphic is anticipated to be most applicable to text 
-- objects where positioning at /baseline-left/ is not always
-- satifactory.
--
--------------------------------------------------------------------------------

module Wumpus.Basic.Kernel.Objects.PosGraphic
  (

    RectPosition(..)
  , ObjectPos(..)
  , BorderRect(..)

  -- * Positionable graphic

  , PosGraphic
  , DPosGraphic 

  , makePosGraphic

  , setPosition

  ) where


import Wumpus.Basic.Kernel.Base.ContextFun
import Wumpus.Basic.Kernel.Objects.BaseObjects
import Wumpus.Basic.Kernel.Objects.Displacement
import Wumpus.Basic.Kernel.Objects.Graphic

import Wumpus.Core                              -- package: wumpus-core


-- | Datatype enumerating positions within a rectangle that can be
-- derived for a 'PosGraphic'.  
--
data RectPosition = CENTER | NN | SS | EE | WW | NE | NW | SE | SW 
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

-- | Border of a PosGraphic.
--
data BorderRect u = BorderRect
      { border_lower_left :: Point2 u
      , border_width      :: u
      , border_height     :: u
      }
  deriving (Eq,Ord,Show)

-- Design note - as BorderRect is synthesized (and potentially not 
-- used) its fields are lazy.


-- | A positionable Graphic.
--
data PosGraphic u = PosGraphic
      { pos_position    :: ObjectPos u
      , pos_image       :: LocImage u (BorderRect u)
      }
       
-- | Version of PosGraphic specialed to Double for the unit type.
--
type DPosGraphic = PosGraphic Double


type instance DUnit (ObjectPos u)   = u
type instance DUnit (BorderRect u)  = u
type instance DUnit (PosGraphic u)  = u


-- TODO - PosGraphic supports a semigroup, but the instance needs 
-- some care. The rect needs semigroup plus the graphics need 
-- their start-points moving (oplus should place the the graphics 
-- on top of each other /retangularly/).


-- | Create a 'PosGraphic'.
--
makePosGraphic :: Num u => ObjectPos u -> LocGraphic u -> PosGraphic u
makePosGraphic opos gf = 
    PosGraphic { pos_position    = opos
               , pos_image       = img  }
  where
    img = promoteR1 $ \pt -> let ans = makeBorderRect opos pt 
                             in replaceAns ans (gf `at` pt)


makeBorderRect :: Num u => ObjectPos u -> Point2 u -> BorderRect u
makeBorderRect (ObjectPos xmin xmaj ymin ymaj) (P2 x y) = 
    BorderRect { border_lower_left = P2 (x - xmin) (y - ymin)
               , border_width      = xmin + xmaj
               , border_height     = ymin + ymaj
               }

  


-- | 'setPosition' : @ start_pos * pos_graphic -> LocImage @
--
-- /Downcast/ a 'PosGraphic' to a 'LocImage' by supplying it 
-- with a 'RectPosition'.
--  
setPosition :: Floating u 
            => RectPosition -> PosGraphic u -> LocImage u (BorderRect u)
setPosition rp (PosGraphic opos mf) = 
    moveStart (displaceVec $ displacement opos rp) mf



displacement :: Fractional u => ObjectPos u -> RectPosition -> Vec2 u
displacement (ObjectPos xminor xmajor yminor ymajor) pos = go pos
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
