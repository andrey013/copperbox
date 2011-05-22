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
  , orientationWidth
  , orientationHeight
 
  , extendOrientation
  , extendOLeft
  , extendORight
  , extendODown
  , extendOUp

  , padHEven
  , padXMinor
  , padXMajor
  , padVEven
  , padYMajor
  , padYMinor

  , spineRight
  , spineBelow

  , halignBottomO
  , halignCenterO
  , halignTopO
  , valignLeftO
  , valignCenterO
  , valignRightO

  , spinemoveH
  , spinemoveV
  , binmoveHBottom
  , binmoveHCenter
  , binmoveHTop
  , binmoveVLeft
  , binmoveVCenter
  , binmoveVRight

  ) where



import Wumpus.Core                              -- package: wumpus-core

import Data.Monoid

-- | Datatype enumerating the addressable positions of a rectangle 
-- that can be derived for a 'PosObject'.  
--
-- The positions are the compass points, plus the geometric 
-- center, origin and the baseline positions: 
-- 
-- > BLL - baseline-left
--
-- > BLC - baseline-center 
-- 
-- > BLR - baseline-right
--
data RectAddress = CENTER | ORIGIN
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



instance (Fractional u, Ord u) => Monoid (Orientation u) where
  mempty  = Orientation 0 0 0 0
  mappend = concatOrientation

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
-- Negate the vector to get from start point to 'RectAddress'.
--
orientationStart :: Fractional u => RectAddress -> Orientation u -> Vec2 u
orientationStart raddr (Orientation xmin xmaj ymin ymaj) = go raddr
  where
    w         = xmin + xmaj
    h         = ymin + ymaj
    hw        = 0.5  * w
    hh        = 0.5  * h
   
    -- CENTER, NN, SS, EE, WW all go to bottomleft then add back 
    -- the minors.

    go CENTER = V2 ((-hw) + xmin) ((-hh) + ymin)
    go ORIGIN = zeroVec
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


-- | Height of the orientation.
--
orientationWidth :: Num u => Orientation u -> u
orientationWidth (Orientation xmin xmaj _ _) = xmin + xmaj

-- | Height of the orientation.
--
orientationHeight :: Num u => Orientation u -> u
orientationHeight (Orientation _ _ ymin ymaj) = ymin + ymaj

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

padHEven :: (Fractional u, Ord u) 
          => u -> Orientation u -> Orientation u
padHEven w ortt@(Orientation xmin xmaj _ _) = 
    if w > ow then ortt { or_x_minor = xmin + hdx
                        , or_x_major = xmaj + hdx } 
              else ortt
  where
    ow = xmin + xmaj
    hdx = 0.5 * (w - ow)


padXMinor :: (Num u, Ord u) 
          => u -> Orientation u -> Orientation u
padXMinor w ortt@(Orientation xmin xmaj _ _) = 
    if w > ow then ortt { or_x_minor = xmin + dx } else ortt
  where
    ow = xmin + xmaj
    dx = w - ow

padXMajor :: (Num u, Ord u)
         => u -> Orientation u -> Orientation u
padXMajor w ortt@(Orientation xmin xmaj _ _) = 
    if w > ow then ortt { or_x_major = xmaj + dx } else ortt
  where
    ow = xmin + xmaj
    dx = w - ow

padVEven :: (Fractional u, Ord u) 
          => u -> Orientation u -> Orientation u
padVEven h ortt@(Orientation _ _ ymin ymaj) = 
    if h > oh then ortt { or_y_minor = ymin + hdy
                        , or_y_major = ymaj + hdy } 
              else ortt
  where
    oh = ymin + ymaj
    hdy = 0.5 * (h - oh)

padYMinor :: (Num u, Ord u) 
         => u -> Orientation u -> Orientation u
padYMinor h ortt@(Orientation _ _ ymin ymaj) = 
    if h > oh then ortt { or_y_minor = ymin + dy } else ortt
  where
    oh = ymin + ymaj
    dy = h - oh


padYMajor :: (Num u, Ord u) 
       => u -> Orientation u -> Orientation u
padYMajor h ortt@(Orientation _ _ ymin ymaj) = 
    if h > oh then ortt { or_y_major = ymaj + dy } else ortt
  where
    oh = ymin + ymaj
    dy = h - oh


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


-- | Second Orientation is moved /below/ the first along the spine
-- i.e. the vertical point between the left minor and right major
-- (not the same as the horizontal center).
--
spineBelow :: (Num u, Ord u) 
           => Orientation u -> Orientation u -> Orientation u
spineBelow (Orientation xmin0 xmaj0 ymin0 ymaj0) 
           (Orientation xmin1 xmaj1 ymin1 ymaj1) = 
    Orientation { or_x_minor = max xmin0 xmin1
                , or_x_major = max xmaj0 xmaj1
                , or_y_minor = ymin0 + ymaj1 + ymin1
                , or_y_major = ymaj0
                }


-- | xmin and xmaj same as left.
--
halignBottomO :: (Num u, Ord u) 
            => Orientation u -> Orientation u -> Orientation u
halignBottomO (Orientation xmin0 xmaj0 ymin0 ymaj0) 
              (Orientation xmin1 xmaj1 ymin1 ymaj1) = 
    let hr = ymin1 + ymaj1
    in Orientation { or_x_minor = xmin0
                   , or_x_major = xmaj0 + xmin1 + xmaj1
                   , or_y_minor = ymin0
                   , or_y_major = max ymaj0 (hr - ymin0)
                   }





-- | xmin same as left.
--
halignCenterO :: (Fractional u, Ord u) 
              => Orientation u -> Orientation u -> Orientation u
halignCenterO (Orientation xmin0 xmaj0 ymin0 ymaj0) 
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
halignTopO :: (Num u, Ord u) 
           => Orientation u -> Orientation u -> Orientation u
halignTopO (Orientation xmin0 xmaj0 ymin0 ymaj0) 
           (Orientation xmin1 xmaj1 ymin1 ymaj1) = 
    let hr = ymin1 + ymaj1
    in Orientation { or_x_minor = xmin0
                   , or_x_major = xmaj0 + xmin1 + xmaj1
                   , or_y_minor = max ymin0 (hr - ymaj0)
                   , or_y_major = ymaj0
                   }

-- | Align second below - xmin and ymaj are same as left.
--
valignLeftO :: (Fractional u, Ord u) 
            => Orientation u -> Orientation u -> Orientation u
valignLeftO (Orientation xmin0 xmaj0 ymin0 ymaj0) 
            (Orientation xmin1 xmaj1 ymin1 ymaj1) = 
    let wr = xmin1 + xmaj1
    in Orientation { or_x_minor = xmin0
                   , or_x_major = max xmaj0 (wr - xmin0)
                   , or_y_minor = ymin0 + ymin1 + ymaj1
                   , or_y_major = ymaj0
                   }



-- | Align second below - ymaj same as left.
--
valignCenterO :: (Fractional u, Ord u) 
             => Orientation u -> Orientation u -> Orientation u
valignCenterO (Orientation xmin0 xmaj0 ymin0 ymaj0) 
              (Orientation xmin1 xmaj1 ymin1 ymaj1) = 
    let wl         = xmin0 + xmaj0
        wr         = xmin1 + xmaj1
        half_diff  = 0.5 * (wr - wl)
    in Orientation 
          { or_x_minor = if wl >= wr then xmin0 else (xmin0 + half_diff)
          , or_x_major = if wl >= wr then xmaj0 else (xmaj0 + half_diff)
          , or_y_minor = ymin0 + ymin1 + ymaj1
          , or_y_major = ymaj0 
          }


-- | Align second below - xmaj and ymaj are same as left.
--
valignRightO :: (Fractional u, Ord u) 
             => Orientation u -> Orientation u -> Orientation u
valignRightO (Orientation xmin0 xmaj0 ymin0 ymaj0) 
             (Orientation xmin1 xmaj1 ymin1 ymaj1) = 
    let wr = xmin1 + xmaj1
    in Orientation { or_x_minor = max xmin0 (wr - xmaj0)
                   , or_x_major = xmaj0 
                   , or_y_minor = ymin0 + ymin1 + ymaj1
                   , or_y_major = ymaj0 
                   }


--------------------------------------------------------------------------------
-- Binary start pos displacement

upDown :: Num u => u -> u -> u
{-# INLINE upDown #-}
upDown u d = u - d

downUp :: Num u => u -> u -> u
{-# INLINE downUp #-}
downUp d u = negate d + u

-- | Move second right.
--
spinemoveH :: Num u => Orientation u -> Orientation u -> Vec2 u
spinemoveH op0 op1 = V2 hdist 0
  where
    hdist = or_x_major op0 + or_x_minor op1

-- | Move second below.
--
spinemoveV :: Num u => Orientation u -> Orientation u -> Vec2 u
spinemoveV op0 op1 = V2 0 (negate vdist)
  where
    vdist = or_y_minor op0 + or_y_major op1
   


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
    vdist = negate $ or_y_minor op0 + or_y_major op1


binmoveVCenter :: (Fractional u, Ord u) 
               => Orientation u -> Orientation u -> Vec2 u
binmoveVCenter (Orientation xmin0 xmaj0 ymin0 _) 
               (Orientation xmin1 xmaj1 _     ymaj1) = 
    V2 hdist vdist
  where
    w0        = xmin0 + xmaj0
    w1        = xmin1 + xmaj1
    half_diff = abs $ 0.5 * (w1 - w0)
    hdist     = if w0 >= w1 then leftRight xmin0 (half_diff + xmin1)
                            else rightLeft (xmaj0 + half_diff) xmaj1
    vdist     = negate $ ymin0 + ymaj1



binmoveVRight :: Num u => Orientation u -> Orientation u -> Vec2 u
binmoveVRight op0 op1 = V2 hdist vdist
  where
    hdist = rightLeft (or_x_major op0) (or_x_major op1)
    vdist = negate $ or_y_minor op0 + or_y_major op1
   