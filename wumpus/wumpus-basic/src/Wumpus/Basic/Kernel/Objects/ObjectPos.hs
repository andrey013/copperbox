{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Kernel.Objects.ObjectPos
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  highly unstable
-- Portability :  GHC 
--
-- ObjectPos is an internal Graphic object for that models
-- rectangular positioning.
--
--------------------------------------------------------------------------------

module Wumpus.Basic.Kernel.Objects.ObjectPos
  (


  -- * Components
    RectPosition(..)
  , ObjectPos(..)

  , objectPosStart
  , objectPosBounds
  , objectPosBoundsLocus

  , extendLeftOP
  , extendRightOP
  , extendDownOP
  , extendUpOP

  , spineRight
  , spineAbove
  , alignBottomR
  , alignCenterR
  , alignTopR
  , alignLeftU
  , alignCenterU
  , alignRightU

  , spinemoveH
  , spinemoveV
  , binmoveHBottom
  , binmoveHCenter
  , binmoveHTop
  , binmoveVLeft
  , binmoveVCenter
  , binmoveVRight

  ) where


import Wumpus.Basic.Kernel.Base.BaseDefs

import Wumpus.Core                              -- package: wumpus-core

import Data.AffineSpace                         -- package: vector-space



-- | Datatype enumerating positions within a rectangle that can be
-- derived for a 'PosGraphic'.  
--
data RectPosition = CENTER 
                  | NN | SS | EE | WW | NE | NW | SE | SW 
                  | BLL | BLC | BLR
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




--------------------------------------------------------------------------------

instance Functor ObjectPos where
  fmap f (ObjectPos xmin xmaj ymin ymaj) = 
    ObjectPos (f xmin) (f xmaj) (f ymin) (f ymaj)

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
{-# INLINE halfDists #-}
halfDists (ObjectPos xmin xmaj ymin ymaj) = 
    (0.5 * (xmin+xmaj), 0.5 * (ymin+ymaj))





-- | The vector from some Rectangle position to the start point.
--
objectPosStart :: Fractional u => RectPosition -> ObjectPos u -> Vec2 u
objectPosStart rpos (ObjectPos xminor xmajor yminor ymajor) = go rpos
  where
    w         = xminor + xmajor
    h         = yminor + ymajor
    hw        = 0.5 * w
    hh        = 0.5 * h
   
    -- CENTER, NN, SS, EE, WW all go to bottomleft then add back 
    -- the minors.

    go CENTER = V2 ((-hw) + xminor) ((-hh) + yminor)
    go NN     = V2 ((-hw) + xminor) ((-h)  + yminor)
    go SS     = V2 ((-hw) + xminor)   yminor
    go EE     = V2 ((-w)  + xminor) ((-hh) + yminor)
    go WW     = V2 xminor           ((-hh) + yminor)
    go NE     = V2 (-xmajor)        (-ymajor)
    go SE     = V2 (-xmajor)          yminor
    go SW     = V2 xminor             yminor
    go NW     = V2 xminor           (-ymajor)
    go BLL    = V2 xminor             0
    go BLC    = V2 ((-hw) + xminor)   0
    go BLR    = V2 ((-w)  + xminor)   0 

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

    -- go finds the bottom-left corner...

    go CENTER = bbox $ P2 (x-hw) (y-hh)
    go NN     = bbox $ P2 (x-hw) (y-h)
    go SS     = bbox $ P2 (x-hw)  y
    go EE     = bbox $ P2 (x-w)  (y-hh)
    go WW     = bbox $ P2  x     (y-hh)
    go NE     = bbox $ P2 (x-w)  (y-h)
    go SE     = bbox $ P2 (x-w)   y
    go SW     = bbox $ P2 x       y
    go NW     = bbox $ P2 x      (y-h)
    go BLL    = bbox $ P2 x       ymin
    go BLC    = bbox $ P2 (x-hw)  ymin
    go BLR    = bbox $ P2 (x-w)   ymin


-- | Calculate the bounding box formed by locating the 'ObjectPos'
-- at the supplied point.
-- 
objectPosBoundsLocus :: Num u 
                     => ObjectPos u -> Point2 u -> BoundingBox u
objectPosBoundsLocus (ObjectPos xmin xmaj ymin ymaj) (P2 x y) = BBox llc urc
  where
    llc   = P2 (x-xmin) (y-ymin)
    urc   = P2 (x+xmaj) (y+ymaj)


--------------------------------------------------------------------------------
-- Extending an arm

extendLeftOP :: Num u => u -> ObjectPos u -> ObjectPos u
extendLeftOP u (ObjectPos xmin xmaj ymin ymaj) = 
    ObjectPos (u+xmin) xmaj ymin ymaj


extendRightOP :: Num u => u -> ObjectPos u -> ObjectPos u
extendRightOP u (ObjectPos xmin xmaj ymin ymaj) = 
    ObjectPos xmin (u+xmaj) ymin ymaj

extendDownOP :: Num u => u -> ObjectPos u -> ObjectPos u
extendDownOP u (ObjectPos xmin xmaj ymin ymaj) = 
    ObjectPos xmin xmaj (u+ymin) ymaj

extendUpOP :: Num u => u -> ObjectPos u -> ObjectPos u
extendUpOP u (ObjectPos xmin xmaj ymin ymaj) = 
    ObjectPos xmin xmaj ymin (u+ymaj)

--------------------------------------------------------------------------------
-- Combining ObjectPos

-- Note - there are lots of concatenations (due to alignment) 
-- we need a consistent name scheme...


-- | Second ObjectPos is moved /to the right/ of the first along
-- the /spine/ i.e the baseline.
--
spineRight :: (Num u, Ord u) 
            => ObjectPos u -> ObjectPos u -> ObjectPos u
spineRight (ObjectPos xmin0 xmaj0 ymin0 ymaj0) 
           (ObjectPos xmin1 xmaj1 ymin1 ymaj1) = 
    ObjectPos { op_x_minor = xmin0
              , op_x_major = xmaj0 + xmin1 + xmaj1 
              , op_y_minor = max ymin0 ymin1
              , op_y_major = max ymaj0 ymaj1
              }


-- | Second ObjectPos is moved /above/ the first along the spine
-- i.e. the vertical point between the left minor and right major
-- (not the same as the horizontal center).
--
spineAbove :: (Num u, Ord u) 
           => ObjectPos u -> ObjectPos u -> ObjectPos u
spineAbove (ObjectPos xmin0 xmaj0 ymin0 ymaj0) 
           (ObjectPos xmin1 xmaj1 ymin1 ymaj1) = 
    ObjectPos { op_x_minor = max xmin0 xmin1
              , op_x_major = max xmaj0 xmaj1
              , op_y_minor = ymin0 
              , op_y_major = ymaj0 + ymin1 + ymaj1
              }



-- | xmin and xmaj same as left.
--
alignBottomR :: (Num u, Ord u) 
             => ObjectPos u -> ObjectPos u -> ObjectPos u
alignBottomR (ObjectPos xmin0 xmaj0 ymin0 ymaj0) 
             (ObjectPos xmin1 xmaj1 ymin1 ymaj1) = 
    let hr = ymin1 + ymaj1
    in ObjectPos { op_x_minor = xmin0
                 , op_x_major = xmaj0 + xmin1 + xmaj1
                 , op_y_minor = ymin0
                 , op_y_major = max ymaj0 (hr - ymin0)
                 }





-- | xmin same as left.
--
alignCenterR :: (Fractional u, Ord u) 
             => ObjectPos u -> ObjectPos u -> ObjectPos u
alignCenterR (ObjectPos xmin0 xmaj0 ymin0 ymaj0) 
             (ObjectPos xmin1 xmaj1 ymin1 ymaj1) = 
    let hl         = ymin0 + ymaj0
        hr         = ymin1 + ymaj1
        half_diff  = 0.5 * (hr - hl)
    in ObjectPos { op_x_minor = xmin0
                 , op_x_major = xmaj0 + xmin1 + xmaj1
                 , op_y_minor = if hl >= hr then ymin0 else (ymin0 + half_diff)
                 , op_y_major = if hl >= hr then ymaj0 else (ymaj0 + half_diff)
                 }



-- | xmin and ymaj same as left.
--
alignTopR :: (Num u, Ord u) 
             => ObjectPos u -> ObjectPos u -> ObjectPos u
alignTopR (ObjectPos xmin0 xmaj0 ymin0 ymaj0) 
          (ObjectPos xmin1 xmaj1 ymin1 ymaj1) = 
    let hr = ymin1 + ymaj1
    in ObjectPos { op_x_minor = xmin0
                 , op_x_major = xmaj0 + xmin1 + xmaj1
                 , op_y_minor = max ymin0 (hr - ymaj0)
                 , op_y_major = ymaj0
                 }

-- | xmin and ymin are same as left.
--
alignLeftU :: (Fractional u, Ord u) 
             => ObjectPos u -> ObjectPos u -> ObjectPos u
alignLeftU (ObjectPos xmin0 xmaj0 ymin0 ymaj0) 
           (ObjectPos xmin1 xmaj1 ymin1 ymaj1) = 
    let wr = xmin1 + xmaj1
    in ObjectPos { op_x_minor = xmin0
                 , op_x_major = max xmaj0 (wr - xmin0)
                 , op_y_minor = ymin0
                 , op_y_major = ymaj0 + ymin1 + ymaj1
                 }



-- | ymin same as left.
--
alignCenterU :: (Fractional u, Ord u) 
             => ObjectPos u -> ObjectPos u -> ObjectPos u
alignCenterU (ObjectPos xmin0 xmaj0 ymin0 ymaj0) 
             (ObjectPos xmin1 xmaj1 ymin1 ymaj1) = 
    let wl         = xmin0 + xmaj0
        wr         = xmin1 + xmaj1
        half_diff  = 0.5 * (wr - wl)
    in ObjectPos { op_x_minor = if wl >= wr then xmin0 else (xmin0 + half_diff)
                 , op_x_major = if wl >= wr then xmaj0 else (xmaj0 + half_diff)
                 , op_y_minor = ymin0 
                 , op_y_major = ymaj0 + ymin1 + ymaj1
                 }


-- | xmaj and ymin are same as left.
--
alignRightU :: (Fractional u, Ord u) 
             => ObjectPos u -> ObjectPos u -> ObjectPos u
alignRightU (ObjectPos xmin0 xmaj0 ymin0 ymaj0) 
            (ObjectPos xmin1 xmaj1 ymin1 ymaj1) = 
    let wr = xmin1 + xmaj1
    in ObjectPos { op_x_minor = max xmin0 (wr - xmaj0)
                 , op_x_major = xmaj0 
                 , op_y_minor = ymin0
                 , op_y_major = ymaj0 + ymin1 + ymaj1
                 }


--------------------------------------------------------------------------------
-- Binary start pos displacement

upDown :: Num u => u -> u -> u
{-# INLINE upDown #-}
upDown u d = u - d

downUp :: Num u => u -> u -> u
{-# INLINE downUp #-}
downUp d u = negate d + u


spinemoveH :: Num u => ObjectPos u -> ObjectPos u -> Vec2 u
spinemoveH op0 op1 = V2 hdist 0
  where
    hdist = op_x_major op0 + op_x_minor op1

spinemoveV :: Num u => ObjectPos u -> ObjectPos u -> Vec2 u
spinemoveV op0 op1 = V2 0 vdist
  where
    vdist = op_y_major op0 + op_y_minor op1
   


binmoveHBottom :: Num u => ObjectPos u -> ObjectPos u -> Vec2 u
binmoveHBottom op0 op1 = V2 hdist vdist
  where
    hdist = op_x_major op0 + op_x_minor op1
    vdist = downUp (op_y_minor op0) (op_y_minor op1)
   

binmoveHCenter :: (Fractional u, Ord u) 
               => ObjectPos u -> ObjectPos u -> Vec2 u
binmoveHCenter (ObjectPos _     xmaj0 ymin0 ymaj0) 
               (ObjectPos xmin1 _     ymin1 ymaj1) = 
    V2 hdist vdist
  where
    h0        = ymin0 + ymaj0
    h1        = ymin1 + ymaj1
    half_diff = abs $ 0.5 * (h1 - h0)
    hdist     = xmaj0 + xmin1
    vdist     = if h0 >= h1 then downUp ymin0 (half_diff + ymin1)
                            else upDown (ymaj0 + half_diff) ymaj1



binmoveHTop :: Num u => ObjectPos u -> ObjectPos u -> Vec2 u
binmoveHTop op0 op1 = V2 hdist vdist
  where
    hdist = op_x_major op0 + op_x_minor op1
    vdist = upDown (op_y_major op0) (op_y_major op1)


leftRight :: Num u => u -> u -> u
{-# INLINE leftRight #-}
leftRight l r = negate l + r


rightLeft :: Num u => u -> u -> u
{-# INLINE rightLeft #-}
rightLeft r l = r - l


binmoveVLeft :: Num u => ObjectPos u -> ObjectPos u -> Vec2 u
binmoveVLeft op0 op1 = V2 hdist vdist
  where
    hdist = leftRight (op_x_minor op0) (op_x_minor op1)
    vdist = op_y_major op0 + op_y_minor op1


binmoveVCenter :: (Fractional u, Ord u) 
               => ObjectPos u -> ObjectPos u -> Vec2 u
binmoveVCenter (ObjectPos xmin0 xmaj0 _     ymaj0) 
               (ObjectPos xmin1 xmaj1 ymin1 _    ) = 
    V2 hdist vdist
  where
    w0        = xmin0 + xmaj0
    w1        = xmin1 + xmaj1
    half_diff = abs $ 0.5 * (w1 - w0)
    hdist     = if w0 >= w1 then leftRight xmin0 (half_diff + xmin1)
                            else rightLeft (xmaj0 + half_diff) xmaj1
    vdist     = ymaj0 + ymin1



binmoveVRight :: Num u => ObjectPos u -> ObjectPos u -> Vec2 u
binmoveVRight op0 op1 = V2 hdist vdist
  where
    hdist = rightLeft (op_x_major op0) (op_x_major op1)
    vdist = op_y_major op0 + op_y_minor op1
   