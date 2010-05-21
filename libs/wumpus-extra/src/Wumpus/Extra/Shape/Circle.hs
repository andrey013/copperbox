{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Extra.Shape.Circle
-- Copyright   :  (c) Stephen Tetley 2009-2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC with TypeFamilies and more
--
-- Coordinate points
-- 
--------------------------------------------------------------------------------

module Wumpus.Extra.Shape.Circle
  ( 

    Circle(..)
  , circle
  , strokeCircle

  ) where

import Wumpus.Core hiding ( CTM )
import Wumpus.Extra.CoreAdditions
import Wumpus.Extra.Shape.Base
import Wumpus.Extra.Utils


--------------------------------------------------------------------------------

-- | Coordinates

data Circle u = Circle
      { circle_radius          :: u
      , circle_center          :: Point2 u
      , circle_ctm             :: CTM u 
      }

type instance DUnit (Circle u) = u


-- helper - extract coord_center w.r.t. the CTM 
--
inCtxCircle :: Num u => Circle u -> (Point2 u -> a) -> a
inCtxCircle coord f = f $ ctm *# pt
    where
      ctm       = circle_ctm    coord
      pt        = circle_center coord

 

-- Instances 

instance Num u => AnchorCenter (Circle u) where
  center c = inCtxCircle c id


instance (Floating u, Real u) => Rotate (Circle u) where
  rotate r = pstar (\m s -> s { circle_ctm = rotateCTM r m }) circle_ctm 

instance (Floating u, Real u) => RotateAbout (Circle u) where
  rotateAbout r pt = 
      pstar (\m s -> s { circle_ctm = rotateAboutCTM r pt m }) circle_ctm


-- For scaling /just/ change the ctm - but remember to 
-- apply the ctm to all points an control points before
-- drawing the circle.
instance Num u => Scale (Circle u) where
  scale x y = pstar2 (\m r s -> s { circle_ctm = scaleCTM x y m
                                  , circle_radius = r }) 
                     circle_ctm 
                     circle_radius

instance Num u => Translate (Circle u) where
  translate x y = 
     pstar (\m s -> s { circle_ctm = translateCTM x y m }) circle_ctm


--

circle :: Num u => u -> Point2 u -> Circle u
circle radius pt = Circle radius pt identityMatrix


-- Hand-build the circle with bezier arcs.
--
-- This makes a better drawing than using PostScript\'s @arc@
-- command (via ellipse in wumpus-core) as line width does not
-- get affected by the CTM.
--
strokeCircle :: (Floating u, Fractional u, Ord u, Stroke t) 
             => t -> Circle u -> Primitive u
strokeCircle t circ = cstroke t $ curvedPath circle_pts
  where
    ctm           = circle_ctm circ
    circle_pts = map (ctm *#) $ 
                      bezierCircle 2 (circle_radius circ) (circle_center circ) 


