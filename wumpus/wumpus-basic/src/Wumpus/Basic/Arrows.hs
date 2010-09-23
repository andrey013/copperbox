{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Arrows
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC with TypeFamilies and more
--
-- Anchor points on shapes.
--
-- \*\* WARNING \*\* this module is an experiment, and may 
-- change significantly or even be dropped from future revisions.
-- 
--------------------------------------------------------------------------------

module Wumpus.Basic.Arrows
  ( 
    line


  , arrowTri90
  , arrowTri60
  , arrowTri45
  , arrowOTri90
  , arrowOTri60
  , arrowOTri45

  , arrowBarb90
  , arrowBarb60
  , arrowBarb45
    
  , arrowPerp

  ) where

import Wumpus.Basic.Arrows.Tips
import Wumpus.Basic.Graphic
import Wumpus.Basic.Paths
import Wumpus.Basic.Utils.Intersection ( langle )

import Wumpus.Core                      -- package: wumpus-core

import Control.Applicative
import Data.Monoid

liftPathF :: PathF u -> ConnDrawingF u (Path u)
liftPathF pF p1 p2 = pureDF $ pF p1 p2

line :: Num u => PathF u -> ConnImage u (Path u)
line pathF = intoConnImage (liftPathF pathF) (pathGraphic pathF)


-- Here the path is already shortened - we have accounted for the
-- points already, so it is just a graphic. 
lineTipR :: Num u => Path u -> Graphic u -> Graphic u
lineTipR bpath tip = openStroke (toPrimPathU bpath) `mappend` tip
   



-- | Returns two items:
-- 
-- 1. Shorten the line by the line width - this stops the path
-- tip puncturing the arrow head (particulary visible on open 
-- triangle tips).
-- 
-- 2. Calculate the direction back along the line at half the 
-- lower_x_height - this gets a good angle for the tip on curved
-- path segments.
--
rightPathProps :: (Real u, Floating u, FromPtSize u) 
               => PathF u -> ConnDrawingF u (Path u,Radian)
rightPathProps pathF p1 p2 = 
    (\h sw -> (shortenPath h sw, calcTheta h))
      <$> markHeight <*> lineWidth
  where
    long_path          = pathF p1 p2  
    shortenPath lxh sw = shortenR (lxh + (realToFrac sw)) long_path 
    calcTheta lxh      = directionR $ shortenR (0.5*lxh) long_path




triTipRight :: (Real u, Floating u, FromPtSize u) 
            => PathF u -> (Radian -> LocGraphic u) -> ConnImage u (Path u) 
triTipRight pathF tipF p1 p2 =
    rightPathProps pathF p1 p2          >>= \(shortF,theta) -> 
    lineTipR shortF (tipF theta p2)     >>= \arrow_pic      ->
    return (pathF p1 p2, arrow_pic)




-- This version does not /retract/ the path...
--
barbTipRight :: (Real u, Floating u, FromPtSize u) 
             => PathF u -> (Radian -> LocGraphic u) -> ConnImage u (Path u)  
barbTipRight pathF tipF p1 p2 = 
    rightPathProps pathF p1 p2          >>= \(_,theta) -> 
    lineTipR path_zero (tipF theta p2)  >>= \arrow_pic  ->
    return (path_zero, arrow_pic)
  where
    path_zero = pathF p1 p2



arrowTri90 :: (Real u, Floating u, FromPtSize u) 
           => PathF u -> ConnImage u (Path u)
arrowTri90 pathF = triTipRight pathF tri90
 
          


arrowTri60 :: (Real u, Floating u, FromPtSize u) 
           => PathF u -> ConnImage u (Path u)
arrowTri60 pathF = triTipRight pathF tri60

 
arrowTri45 :: (Real u, Floating u, FromPtSize u) 
           => PathF u -> ConnImage u (Path u)
arrowTri45 pathF = triTipRight pathF tri45


arrowOTri90 :: (Real u, Floating u, FromPtSize u) 
            => PathF u -> ConnImage u (Path u)
arrowOTri90 pathF = triTipRight pathF otri90
     

arrowOTri60 :: (Real u, Floating u, FromPtSize u) 
            => PathF u -> ConnImage u (Path u)
arrowOTri60 pathF = triTipRight pathF otri60 



arrowOTri45 :: (Real u, Floating u, FromPtSize u) 
            => PathF u -> ConnImage u (Path u)
arrowOTri45 pathF = triTipRight pathF otri45



arrowBarb90 :: (Real u, Floating u, FromPtSize u) 
            => PathF u -> ConnImage u (Path u)
arrowBarb90 pathF = barbTipRight pathF barb90

arrowBarb60 :: (Real u, Floating u, FromPtSize u) 
            => PathF u -> ConnImage u (Path u)
arrowBarb60 pathF = barbTipRight pathF barb60

arrowBarb45 :: (Real u, Floating u, FromPtSize u) 
            => PathF u -> ConnImage u (Path u)
arrowBarb45 pathF = barbTipRight pathF barb45


                     
arrowPerp :: (Real u, Floating u, FromPtSize u) 
          => PathF u -> ConnImage u (Path u)
arrowPerp pathF p1 p2 = 
    lineTipR path_zero perp_tip >>= \arrow_pic -> return (path_zero, arrow_pic)
  where
    path_zero = pathF  p1 p2
    theta     = langle p1 p2
    perp_tip  = perp theta p2
