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

  , emptyPosGraphic
  , makePosGraphic

  , startPosition

  , hplus
  , vplus

  , hspace
  , vspace

  , hcat
  , vcat
  , hsep
  , vsep

  ) where


import Wumpus.Basic.Kernel.Base.BaseDefs
import Wumpus.Basic.Kernel.Base.ContextFun
import Wumpus.Basic.Kernel.Objects.BaseObjects
import Wumpus.Basic.Kernel.Objects.Displacement
import Wumpus.Basic.Kernel.Objects.Graphic

import Wumpus.Core                              -- package: wumpus-core


import Data.List ( foldl' )


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


--------------------------------------------------------------------------------

instance (Num u, Ord u) => OPlus (BorderRect u) where
  BorderRect (P2 x0 y0) w0 h0 `oplus` BorderRect (P2 x1 y1) w1 h1 = 
     let bl     = P2 (min x0 x1) (min y0 y1)
         tr     = maxPt (P2 (x0+w0) (y0+h0)) (P2 (x1+w1) (y1+h1))
         V2 w h = pvec bl tr
     in BorderRect bl w h

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

instance (Floating u, Ord u) => OPlus (PosGraphic u) where
  oplus = concatPosGraphic                           


-- | Concatenate the 'ObjectPos' parts of the 'PosGraphic' args by 
-- putting them in /center-form/. Draw both graphics at the 
-- center, return a 'BorderRect' in /center-form.
--
concatPosGraphic :: (Floating u, Ord u) 
                 => PosGraphic u -> PosGraphic u -> PosGraphic u
concatPosGraphic pg0@(PosGraphic opos0 _) pg1@(PosGraphic opos1 _) = 
    PosGraphic opos img
  where
    opos = opos0 `oplus` opos1
    img  = promoteR1 $ \ctr -> let body = atCenter pg0 `oplus` atCenter pg1
                                   ans  = makeBorderRect opos ctr
                               in replaceAns ans $ body `at` ctr
 

--------------------------------------------------------------------------------

emptyPosGraphic :: Num u => PosGraphic u
emptyPosGraphic = makePosGraphic emptyObjectPos emptyLocGraphic

emptyObjectPos :: Num u => ObjectPos u
emptyObjectPos = ObjectPos 0 0 0 0

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

atCenter :: Floating u => PosGraphic u -> LocGraphic u 
atCenter gf = ignoreAns $ startPosition CENTER gf


-- | 'startPosition' : @ start_pos * pos_graphic -> LocImage @
--
-- /Downcast/ a 'PosGraphic' to a 'LocImage' by supplying it 
-- with a 'RectPosition' (start position).
--  
startPosition :: Floating u 
              => RectPosition -> PosGraphic u -> LocImage u (BorderRect u)
startPosition rp (PosGraphic opos mf) = 
    moveStart (displaceVec $ startVector rp opos) mf


-- | The vector from some Rectangle position to the start point.
--
startVector :: Fractional u => RectPosition -> ObjectPos u -> Vec2 u
startVector pos (ObjectPos xminor xmajor yminor ymajor) = go pos
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


--------------------------------------------------------------------------------
-- Concatenation

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



-- | Concatenation here essentially turns both ObjectPos objects
-- into /center-form/ then finds the rectangle: 
-- 
-- > both widths, max height.
--
hsepObjectPos :: (Fractional u, Ord u) 
              => u -> ObjectPos u -> ObjectPos u -> ObjectPos u
hsepObjectPos dx op0 op1 = ObjectPos hw hw hh hh
  where
    (hw0,hh0) = halfDists op0
    (hw1,hh1) = halfDists op1
    hw        = hw0 + (0.5*dx) + hw1
    hh        = max hh0 hh1



-- | Concatenate the 'ObjectPos' parts of the 'PosGraphic' args by 
-- putting them in /center-form/. Displace the respective 
-- start-points by the distance from the new center to the old 
-- center, and draw with the center at the displaced start-point.
--
hplus :: (Floating u, Ord u) 
      => PosGraphic u -> PosGraphic u -> PosGraphic u
hplus = hspace 0

-- | 'hspace' : @ space * pos_graphic1 * pos_graphic2 -> PosGraphic @
--
hspace :: (Floating u, Ord u) 
       => u -> PosGraphic u -> PosGraphic u -> PosGraphic u
hspace dx pg0@(PosGraphic opos0 _) pg1@(PosGraphic opos1 _) = 
    PosGraphic opos img
  where
    opos = hsepObjectPos dx opos0 opos1

    -- Find the distances from the combined center 
    -- to the /old/ centers...
    --
    hw0  = 0.5 * (op_x_minor opos0 + op_x_major opos0)
    cc0  = op_x_minor opos - hw0
    hw1  = 0.5 * (op_x_minor opos1 + op_x_major opos1)
    cc1  = op_x_minor opos - hw1

    img  = promoteR1 $ \ctr -> 
              let mv0  = moveStart (displaceH $ negate cc0)
                  mv1  = moveStart (displaceH $ cc1)
                  gf  = mv0 (atCenter pg0) `oplus` mv1 (atCenter pg1)
                  ans = makeBorderRect opos ctr
              in replaceAns ans $ gf `at` ctr





-- | Concatenation here essentially turns both ObjectPos objects
-- into /center-form/ then finds the rectangle: 
-- 
-- > max widths, both heights.
--
vsepObjectPos :: (Fractional u, Ord u) 
              => u -> ObjectPos u -> ObjectPos u -> ObjectPos u
vsepObjectPos dy op0 op1 = ObjectPos hw hw hh hh
  where
    (hw0,hh0) = halfDists op0
    (hw1,hh1) = halfDists op1
    hw        = max hw0 hw1
    hh        = hh0 + (0.5*dy) + hh1


-- | Concatenate the 'ObjectPos' parts of the 'PosGraphic' args by 
-- putting them in /center-form/. Displace the respective 
-- start-points by the distance from the new center to the old 
-- center, and draw with the center at the displaced start-point.
--
vplus :: (Floating u, Ord u) 
      => PosGraphic u -> PosGraphic u -> PosGraphic u
vplus = vspace 0

-- | 'vspace'
--
vspace :: (Floating u, Ord u) 
       => u -> PosGraphic u -> PosGraphic u -> PosGraphic u
vspace dy pg0@(PosGraphic opos0 _) pg1@(PosGraphic opos1 _) = 
    PosGraphic opos img
  where
    opos = vsepObjectPos dy opos0 opos1

    -- Find the distances from the combined center 
    -- to the /old/ centers...
    --
    hh0  = 0.5 * (op_y_minor opos0 + op_y_major opos0)
    cc0  = op_y_minor opos - hh0
    hh1  = 0.5 * (op_y_minor opos1 + op_y_major opos1)
    cc1  = op_y_minor opos - hh1

    img  = promoteR1 $ \ctr -> 
              let mv0  = moveStart (displaceV $ cc0)
                  mv1  = moveStart (displaceV $ negate cc1)
                  gf  = mv0 (atCenter pg0) `oplus` mv1 (atCenter pg1)
                  ans = makeBorderRect opos ctr
              in replaceAns ans $ gf `at` ctr

-- | 'hcat' :
--
hcat :: (Floating u, Ord u) => [PosGraphic u] -> PosGraphic u
hcat []     = emptyPosGraphic
hcat (a:as) = foldl' hplus a as



-- | 'vcat' :
--
vcat :: (Floating u, Ord u) => [PosGraphic u] -> PosGraphic u
vcat []     = emptyPosGraphic
vcat (a:as) = foldl' vplus a as

-- | 'hsep' :
--
hsep :: (Floating u, Ord u) => u -> [PosGraphic u] -> PosGraphic u
hsep _  []     = emptyPosGraphic
hsep dx (a:as) = foldl' (hspace dx) a as

-- | 'vsep' :
--
vsep :: (Floating u, Ord u) => u -> [PosGraphic u] -> PosGraphic u
vsep _  []     = emptyPosGraphic
vsep dy (a:as) = foldl' (vspace dy) a as
