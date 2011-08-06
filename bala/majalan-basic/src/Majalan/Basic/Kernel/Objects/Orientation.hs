{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Majalan.Basic.Kernel.Objects.Orientation
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  highly unstable
-- Portability :  GHC 
--
-- Objects LinearAddress and Orientation to model positioning of 
-- an event.
-- 
-- Note - this is a transliteration from Wumpus, where drawing
-- objects have an obvious need for positioning. Potentially this
-- is superfluous for events in Majalan.
--
--------------------------------------------------------------------------------

module Majalan.Basic.Kernel.Objects.Orientation
  (


  -- * Components
    LinearAddress(..)
  , Orientation(..)

  , vtoAddress
  , vtoOrigin
  , orientationBounds
  , orientationWidth
 
  , extendOrientation
  , extendOLeft
  , extendORight

  , padHEven
  , padXMinor
  , padXMajor


  , mergeOrigins
  , mergeCenters
  , mergeHLine

  ) where



import Majalan.Core                             -- package: majalan-core

import Data.Monoid

-- | Datatype enumerating the addressable positions of a line
-- that can be derived for a 'PosObject'.  
--
-- The positions are the center, origin (not necessarily the left
-- edge) and left (start) and right (end).
-- 
data LinearAddress = CENTER | ORIGIN | LEFT | RIGHT
  deriving (Enum,Eq,Ord,Show)




-- | Utility datatype representing orientation within a 
-- linear /frame/. 
--
-- > x_minor is the horizontal distance from the left to the start point
-- >
-- > x_major is the horizontal distance from the start point to the right
-- >
-- Values should be not be negative!
--
-- 
data Orientation u = Orientation
      { or_x_minor      :: !u
      , or_x_major      :: !u
      }
  deriving (Eq,Ord,Show)

type instance DUnit (Orientation u) = u

instance Functor Orientation where
  fmap f (Orientation xmin xmaj) = Orientation (f xmin) (f xmaj)



-- Monoid is @merge-center-center@
--
instance (Num u, Ord u) => Monoid (Orientation u) where
  mempty  = Orientation 0 0 
  Orientation xmin0 xmaj0 `mappend` Orientation xmin1 xmaj1 = 
    Orientation (max xmin0 xmin1) (max xmaj0 xmaj1)
      


--------------------------------------------------------------------------------





vtoAddress :: (Fractional u, Ord u)
           => Orientation u -> LinearAddress -> u
vtoAddress (Orientation xmin xmaj) = step
  where
    hw  = 0.5 * (xmin + xmaj)

    -- > [..o..^.....]  , o -> ^
    --
    step CENTER = if xmin < xmaj then hw - xmin else negate (xmin - hw)

    -- > [..o..^.....]  , o -> o
    --
    step ORIGIN = 0

    -- > [..o..^.....]  , o -> [
    --
    step LEFT   = negate xmin
    
    -- > [..o..^.....]  , o -> ]
    --
    step RIGHT  = xmaj
    
vtoOrigin :: (Fractional u, Ord u) 
          => LinearAddress -> Orientation u -> u
vtoOrigin addr ortt = negate $ vtoAddress ortt addr




-- | Calculate the timespan formed by locating the 'Orientation'
-- at the supplied position.
-- 
orientationBounds :: Num u 
                  => Orientation u -> u -> Timespan u
orientationBounds (Orientation xmin xmaj) x = Timespan (x - xmin) (x + xmaj)


-- | Height of the orientation.
--
orientationWidth :: Num u => Orientation u -> u
orientationWidth (Orientation xmin xmaj) = xmin + xmaj

--------------------------------------------------------------------------------
-- Extending an arm of the orientation

extendOrientation :: Num u 
                  => u -> u -> Orientation u -> Orientation u
extendOrientation dxl dxr (Orientation xmin xmaj) = 
    Orientation (xmin+dxl) (xmaj+dxr) 

extendOLeft :: Num u => u -> Orientation u -> Orientation u
extendOLeft u (Orientation xmin xmaj) = Orientation (u+xmin) xmaj 


extendORight :: Num u => u -> Orientation u -> Orientation u
extendORight u (Orientation xmin xmaj) = Orientation xmin (u+xmaj)


--------------------------------------------------------------------------------

padHEven :: (Fractional u, Ord u) 
          => u -> Orientation u -> Orientation u
padHEven w ortt@(Orientation xmin xmaj) = 
    if w > ow then ortt { or_x_minor = xmin + hdx
                        , or_x_major = xmaj + hdx } 
              else ortt
  where
    ow = xmin + xmaj
    hdx = 0.5 * (w - ow)


padXMinor :: (Num u, Ord u) 
          => u -> Orientation u -> Orientation u
padXMinor w ortt@(Orientation xmin xmaj) = 
    if w > ow then ortt { or_x_minor = xmin + dx } else ortt
  where
    ow = xmin + xmaj
    dx = w - ow

padXMajor :: (Num u, Ord u)
         => u -> Orientation u -> Orientation u
padXMajor w ortt@(Orientation xmin xmaj) = 
    if w > ow then ortt { or_x_major = xmaj + dx } else ortt
  where
    ow = xmin + xmaj
    dx = w - ow


--------------------------------------------------------------------------------
-- Combining Orientation

type MergeResult u = (u, Orientation u)
    

mergeOrigins :: (Num u, Ord u)
             => Orientation u -> Orientation u -> MergeResult u
mergeOrigins (Orientation xmin0 xmaj0) (Orientation xmin1 xmaj1) =
  (0, Orientation (max xmin0 xmin1) (max xmaj0 xmaj1))



mergeCenters :: (Fractional u, Ord u)
             => Orientation u -> Orientation u -> MergeResult u
mergeCenters ortt0@(Orientation xmin0 xmaj0) ortt1@(Orientation xmin1 xmaj1) =
    (move, ortt)
  where
    move   = vtoAddress ortt0 CENTER - vtoOrigin CENTER ortt1
    width0 = xmin0 + xmaj0 
    width1 = xmin1 + xmaj1

    ortt   = if width0 >= width1 then ortt0
                                 else padHEven (width1 - width0) ortt1


-- | Merge end-to-end in a horizontal line. Second is moved.
--
mergeHLine :: Num u => Orientation u -> Orientation u -> MergeResult u
mergeHLine (Orientation xmin0 xmaj0) (Orientation xmin1 xmaj1) = 
    (move, Orientation xmin0 (xmaj0 + xmin1 + xmaj1))
  where
    move = xmaj0 + xmin1
    

