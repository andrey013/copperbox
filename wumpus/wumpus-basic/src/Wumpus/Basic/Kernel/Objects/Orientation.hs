{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Kernel.Objects.Orientation
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  highly unstable
-- Portability :  GHC 
--
-- Graphic objects RectAddress and Orientation to model 
-- rectangular positioning.
--
--------------------------------------------------------------------------------

module Wumpus.Basic.Kernel.Objects.Orientation
  (


  -- * Components
    RectAddress(..)
  , Orientation(..)

  , orientationStart
  , orientationBounds
 
  , extendOrientation
  , extendOLeft
  , extendORight
  , extendODown
  , extendOUp

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



-- | Datatype enumerating the addressable positions of a rectangle 
-- that can be derived for a 'PosObject'.  
--
data RectAddress = CENTER 
                 | NN | SS | EE | WW | NE | NW | SE | SW 
                 | BLL | BLC | BLR
  deriving (Enum,Eq,Ord,Show)




-- | Utility datatype representing orientation within a 
-- rectangular /frame/. RectPos is useful for graphics such as 
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
data Orientation u = Orientation
      { or_x_minor      :: !u
      , or_x_major      :: !u
      , or_y_minor      :: !u
      , or_y_major      :: !u
      }
  deriving (Eq,Ord,Show)




--------------------------------------------------------------------------------

instance Functor Orientation where
  fmap f (Orientation xmin xmaj ymin ymaj) = 
    Orientation (f xmin) (f xmaj) (f ymin) (f ymaj)

instance (Fractional u, Ord u) => OPlus (Orientation u) where
  oplus = concatOrientation


-- | Concatenation here essentially turns both Orientation objects
-- into /center-form/ then finds the maximum rectangle.
--
concatOrientation :: (Fractional u, Ord u) 
                => Orientation u -> Orientation u -> Orientation u
concatOrientation op0 op1 = Orientation hw hw hh hh
  where
    (hw0,hh0) = halfDists op0
    (hw1,hh1) = halfDists op1
    hw        = max hw0 hw1
    hh        = max hh0 hh1




-- | Find the half-width and half-height of an Orientation.
-- 
-- Essentially this is /center-form/ of an Orientation, but 
-- in /center-form/ there is duplication: 
--
-- > xminor == xmajor
-- > yminor == ymajor
-- 
-- So instead, the result type is just a pair.
--
halfDists :: Fractional u => Orientation u -> (u,u)
{-# INLINE halfDists #-}
halfDists (Orientation xmin xmaj ymin ymaj) = 
    (0.5 * (xmin+xmaj), 0.5 * (ymin+ymaj))





-- | The vector from a 'RectAddress' to the start point.
--
orientationStart :: Fractional u => RectAddress -> Orientation u -> Vec2 u
orientationStart rpos (Orientation xmin xmaj ymin ymaj) = go rpos
  where
    w         = xmin + xmaj
    h         = ymin + ymaj
    hw        = 0.5 * w
    hh        = 0.5 * h
   
    -- CENTER, NN, SS, EE, WW all go to bottomleft then add back 
    -- the minors.

    go CENTER = V2 ((-hw) + xmin) ((-hh) + ymin)
    go NN     = V2 ((-hw) + xmin) ((-h)  + ymin)
    go SS     = V2 ((-hw) + xmin)   ymin
    go EE     = V2 ((-w)  + xmin) ((-hh) + ymin)
    go WW     = V2 xmin           ((-hh) + ymin)
    go NE     = V2 (-xmaj)        (-ymaj)
    go SE     = V2 (-xmaj)          ymin
    go SW     = V2 xmin             ymin
    go NW     = V2 xmin           (-ymaj)
    go BLL    = V2 xmin             0
    go BLC    = V2 ((-hw) + xmin)   0
    go BLR    = V2 ((-w)  + xmin)   0 


-- | Calculate the bounding box formed by locating the 'Orientation'
-- at the supplied point.
-- 
orientationBounds :: Num u 
                  => Orientation u -> Point2 u -> BoundingBox u
orientationBounds (Orientation xmin xmaj ymin ymaj) (P2 x y) = BBox llc urc
  where
    llc   = P2 (x-xmin) (y-ymin)
    urc   = P2 (x+xmaj) (y+ymaj)


--------------------------------------------------------------------------------
-- Extending an arm of the orientation

extendOrientation :: Num u 
                  => u -> u -> u -> u -> Orientation u -> Orientation u
extendOrientation dxl dxr dyd dyu (Orientation xmin xmaj ymin ymaj) = 
    Orientation (xmin+dxl) (xmaj+dxr) (ymin+dyd) (ymaj+dyu)

extendOLeft :: Num u => u -> Orientation u -> Orientation u
extendOLeft u (Orientation xmin xmaj ymin ymaj) = 
    Orientation (u+xmin) xmaj ymin ymaj


extendORight :: Num u => u -> Orientation u -> Orientation u
extendORight u (Orientation xmin xmaj ymin ymaj) = 
    Orientation xmin (u+xmaj) ymin ymaj

extendODown :: Num u => u -> Orientation u -> Orientation u
extendODown u (Orientation xmin xmaj ymin ymaj) = 
    Orientation xmin xmaj (u+ymin) ymaj

extendOUp :: Num u => u -> Orientation u -> Orientation u
extendOUp u (Orientation xmin xmaj ymin ymaj) = 
    Orientation xmin xmaj ymin (u+ymaj)

--------------------------------------------------------------------------------
-- Combining Orientation

-- Note - there are lots of concatenations (due to alignment) 
-- we need a consistent name scheme...


-- | Second Orientation is moved /to the right/ of the first along
-- the /spine/ i.e the baseline.
--
spineRight :: (Num u, Ord u) 
            => Orientation u -> Orientation u -> Orientation u
spineRight (Orientation xmin0 xmaj0 ymin0 ymaj0) 
           (Orientation xmin1 xmaj1 ymin1 ymaj1) = 
    Orientation { or_x_minor = xmin0
                , or_x_major = xmaj0 + xmin1 + xmaj1 
                , or_y_minor = max ymin0 ymin1
                , or_y_major = max ymaj0 ymaj1
                }


-- | Second Orientation is moved /above/ the first along the spine
-- i.e. the vertical point between the left minor and right major
-- (not the same as the horizontal center).
--
spineAbove :: (Num u, Ord u) 
           => Orientation u -> Orientation u -> Orientation u
spineAbove (Orientation xmin0 xmaj0 ymin0 ymaj0) 
           (Orientation xmin1 xmaj1 ymin1 ymaj1) = 
    Orientation { or_x_minor = max xmin0 xmin1
                , or_x_major = max xmaj0 xmaj1
                , or_y_minor = ymin0 
                , or_y_major = ymaj0 + ymin1 + ymaj1
                }



-- | xmin and xmaj same as left.
--
alignBottomR :: (Num u, Ord u) 
             => Orientation u -> Orientation u -> Orientation u
alignBottomR (Orientation xmin0 xmaj0 ymin0 ymaj0) 
             (Orientation xmin1 xmaj1 ymin1 ymaj1) = 
    let hr = ymin1 + ymaj1
    in Orientation { or_x_minor = xmin0
                   , or_x_major = xmaj0 + xmin1 + xmaj1
                   , or_y_minor = ymin0
                   , or_y_major = max ymaj0 (hr - ymin0)
                   }





-- | xmin same as left.
--
alignCenterR :: (Fractional u, Ord u) 
             => Orientation u -> Orientation u -> Orientation u
alignCenterR (Orientation xmin0 xmaj0 ymin0 ymaj0) 
             (Orientation xmin1 xmaj1 ymin1 ymaj1) = 
    let hl         = ymin0 + ymaj0
        hr         = ymin1 + ymaj1
        half_diff  = 0.5 * (hr - hl)
    in Orientation 
          { or_x_minor = xmin0
          , or_x_major = xmaj0 + xmin1 + xmaj1
          , or_y_minor = if hl >= hr then ymin0 else (ymin0 + half_diff)
          , or_y_major = if hl >= hr then ymaj0 else (ymaj0 + half_diff)
          }



-- | xmin and ymaj same as left.
--
alignTopR :: (Num u, Ord u) 
             => Orientation u -> Orientation u -> Orientation u
alignTopR (Orientation xmin0 xmaj0 ymin0 ymaj0) 
          (Orientation xmin1 xmaj1 ymin1 ymaj1) = 
    let hr = ymin1 + ymaj1
    in Orientation { or_x_minor = xmin0
                   , or_x_major = xmaj0 + xmin1 + xmaj1
                   , or_y_minor = max ymin0 (hr - ymaj0)
                   , or_y_major = ymaj0
                   }

-- | xmin and ymin are same as left.
--
alignLeftU :: (Fractional u, Ord u) 
             => Orientation u -> Orientation u -> Orientation u
alignLeftU (Orientation xmin0 xmaj0 ymin0 ymaj0) 
           (Orientation xmin1 xmaj1 ymin1 ymaj1) = 
    let wr = xmin1 + xmaj1
    in Orientation { or_x_minor = xmin0
                   , or_x_major = max xmaj0 (wr - xmin0)
                   , or_y_minor = ymin0
                   , or_y_major = ymaj0 + ymin1 + ymaj1
                   }



-- | ymin same as left.
--
alignCenterU :: (Fractional u, Ord u) 
             => Orientation u -> Orientation u -> Orientation u
alignCenterU (Orientation xmin0 xmaj0 ymin0 ymaj0) 
             (Orientation xmin1 xmaj1 ymin1 ymaj1) = 
    let wl         = xmin0 + xmaj0
        wr         = xmin1 + xmaj1
        half_diff  = 0.5 * (wr - wl)
    in Orientation 
          { or_x_minor = if wl >= wr then xmin0 else (xmin0 + half_diff)
          , or_x_major = if wl >= wr then xmaj0 else (xmaj0 + half_diff)
          , or_y_minor = ymin0 
          , or_y_major = ymaj0 + ymin1 + ymaj1
          }


-- | xmaj and ymin are same as left.
--
alignRightU :: (Fractional u, Ord u) 
             => Orientation u -> Orientation u -> Orientation u
alignRightU (Orientation xmin0 xmaj0 ymin0 ymaj0) 
            (Orientation xmin1 xmaj1 ymin1 ymaj1) = 
    let wr = xmin1 + xmaj1
    in Orientation { or_x_minor = max xmin0 (wr - xmaj0)
                   , or_x_major = xmaj0 
                   , or_y_minor = ymin0
                   , or_y_major = ymaj0 + ymin1 + ymaj1
                   }


--------------------------------------------------------------------------------
-- Binary start pos displacement

upDown :: Num u => u -> u -> u
{-# INLINE upDown #-}
upDown u d = u - d

downUp :: Num u => u -> u -> u
{-# INLINE downUp #-}
downUp d u = negate d + u


spinemoveH :: Num u => Orientation u -> Orientation u -> Vec2 u
spinemoveH op0 op1 = V2 hdist 0
  where
    hdist = or_x_major op0 + or_x_minor op1

spinemoveV :: Num u => Orientation u -> Orientation u -> Vec2 u
spinemoveV op0 op1 = V2 0 vdist
  where
    vdist = or_y_major op0 + or_y_minor op1
   


binmoveHBottom :: Num u => Orientation u -> Orientation u -> Vec2 u
binmoveHBottom op0 op1 = V2 hdist vdist
  where
    hdist = or_x_major op0 + or_x_minor op1
    vdist = downUp (or_y_minor op0) (or_y_minor op1)
   

binmoveHCenter :: (Fractional u, Ord u) 
               => Orientation u -> Orientation u -> Vec2 u
binmoveHCenter (Orientation _     xmaj0 ymin0 ymaj0) 
               (Orientation xmin1 _     ymin1 ymaj1) = 
    V2 hdist vdist
  where
    h0        = ymin0 + ymaj0
    h1        = ymin1 + ymaj1
    half_diff = abs $ 0.5 * (h1 - h0)
    hdist     = xmaj0 + xmin1
    vdist     = if h0 >= h1 then downUp ymin0 (half_diff + ymin1)
                            else upDown (ymaj0 + half_diff) ymaj1



binmoveHTop :: Num u => Orientation u -> Orientation u -> Vec2 u
binmoveHTop op0 op1 = V2 hdist vdist
  where
    hdist = or_x_major op0 + or_x_minor op1
    vdist = upDown (or_y_major op0) (or_y_major op1)


leftRight :: Num u => u -> u -> u
{-# INLINE leftRight #-}
leftRight l r = negate l + r


rightLeft :: Num u => u -> u -> u
{-# INLINE rightLeft #-}
rightLeft r l = r - l


binmoveVLeft :: Num u => Orientation u -> Orientation u -> Vec2 u
binmoveVLeft op0 op1 = V2 hdist vdist
  where
    hdist = leftRight (or_x_minor op0) (or_x_minor op1)
    vdist = or_y_major op0 + or_y_minor op1


binmoveVCenter :: (Fractional u, Ord u) 
               => Orientation u -> Orientation u -> Vec2 u
binmoveVCenter (Orientation xmin0 xmaj0 _     ymaj0) 
               (Orientation xmin1 xmaj1 ymin1 _    ) = 
    V2 hdist vdist
  where
    w0        = xmin0 + xmaj0
    w1        = xmin1 + xmaj1
    half_diff = abs $ 0.5 * (w1 - w0)
    hdist     = if w0 >= w1 then leftRight xmin0 (half_diff + xmin1)
                            else rightLeft (xmaj0 + half_diff) xmaj1
    vdist     = ymaj0 + ymin1



binmoveVRight :: Num u => Orientation u -> Orientation u -> Vec2 u
binmoveVRight op0 op1 = V2 hdist vdist
  where
    hdist = rightLeft (or_x_major op0) (or_x_major op1)
    vdist = or_y_major op0 + or_y_minor op1
   