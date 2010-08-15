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
  
  , arrowPerp

  ) where

import Wumpus.Basic.Arrows.Tips
import Wumpus.Basic.Graphic
import Wumpus.Basic.Graphic.DrawingAttr
import Wumpus.Basic.Monads.Drawing
import Wumpus.Basic.Paths
import Wumpus.Basic.Paths.Base

import Wumpus.Core                      -- package: wumpus-core




arrowWidth :: FromPtSize u => DrawingAttr -> u 
arrowWidth = fromPtSize . xcharHeight . font_size . font_props


line :: Num u => BPathF u -> AConnector u (BPath u)
line pathF p0 p1 = AGraphic df mf
  where
    df attr () = pathGraphic (pathF p0 p1) attr
    mf _    () = pathF p0 p1




arrowTri90 :: (Real u, Floating u, FromPtSize u) 
           => BPathF u -> AConnector u (BPath u)
arrowTri90 pathF = \p0 p1 -> 
    AGraphic (\attr () -> mkDF_tipR pathF tri90 p0 p1 attr) 
             (\_    () -> pathF p0 p1)

arrowTri60 :: (Real u, Floating u, FromPtSize u) 
           => BPathF u -> AConnector u (BPath u)
arrowTri60 pathF = \p0 p1 ->
    AGraphic (\attr () -> mkDF_tipR pathF tri60 p0 p1 attr) 
             (\_    () -> pathF p0 p1)
 
arrowTri45 :: (Real u, Floating u, FromPtSize u) 
           => BPathF u -> AConnector u (BPath u)
arrowTri45 pathF = \p0 p1 ->
    AGraphic (\attr () -> mkDF_tipR pathF tri45 p0 p1 attr) 
             (\_    () -> pathF p0 p1)

arrowOTri90 :: (Real u, Floating u, FromPtSize u) 
            => BPathF u -> AConnector u (BPath u)
arrowOTri90 pathF = \p0 p1 ->
    AGraphic (\attr () -> mkDF_tipR pathF otri90 p0 p1 attr) 
             (\_    () -> pathF p0 p1)

arrowOTri60 :: (Real u, Floating u, FromPtSize u) 
            => BPathF u -> AConnector u (BPath u)
arrowOTri60 pathF = \p0 p1 -> 
    AGraphic (\attr () -> mkDF_tipR pathF otri60 p0 p1 attr) 
             (\_    () -> pathF p0 p1)


arrowOTri45 :: (Real u, Floating u, FromPtSize u) 
            => BPathF u -> AConnector u (BPath u)
arrowOTri45 pathF = \p0 p1 ->
    AGraphic (\attr () -> mkDF_tipR pathF otri45 p0 p1 attr)
             (\_    () -> pathF p0 p1)




mkDF_tipR :: (Real u, Floating u, FromPtSize u) 
          => BPathF u 
          -> (Radian -> DrawingAttr -> GraphicF u)
          -> Point2 u -> Point2 u -> DrawingAttr 
          -> Graphic u 
mkDF_tipR pathF tipF p0 p1 attr = 
    pathGraphic short_path attr . tipF theta attr p1
  where
    sz          = arrowWidth attr
    long_path   = pathF p0 p1
    short_path  = shortenR sz long_path
    theta       = directionR long_path
                     



arrowPerp :: (Real u, Floating u, FromPtSize u) 
          => BPathF u -> AConnector u (BPath u)
arrowPerp pathF p0 p1 = AGraphic df mf
  where
    df attr () = let theta = langle p0 p1  in
                 pathGraphic (pathF p0 p1) attr . perp theta attr p1
    mf _    () = pathF p0 p1

