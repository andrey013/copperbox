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


line :: Num u => BPathF u -> AGraphic2 u (BPath u)
line pathF = AGraphic2 id df mf
  where
    df attr p0 p1 = pathGraphic (pathF p0 p1) attr
    mf _    p0 p1 = pathF p0 p1



arrowTri90 :: (Real u, Floating u, FromPtSize u) 
           => BPathF u -> AGraphic2 u (BPath u)
arrowTri90 pathF = AGraphic2 id (mkDF_tipR pathF tri90) (\_ -> pathF)

arrowTri60 :: (Real u, Floating u, FromPtSize u) 
           => BPathF u -> AGraphic2 u (BPath u)
arrowTri60 pathF = AGraphic2 id (mkDF_tipR pathF tri60) (\_ -> pathF)
 
arrowTri45 :: (Real u, Floating u, FromPtSize u) 
           => BPathF u -> AGraphic2 u (BPath u)
arrowTri45 pathF = AGraphic2 id (mkDF_tipR pathF tri45) (\_ -> pathF)

arrowOTri90 :: (Real u, Floating u, FromPtSize u) 
            => BPathF u -> AGraphic2 u (BPath u)
arrowOTri90 pathF = AGraphic2 id (mkDF_tipR pathF otri90) (\_ -> pathF)

arrowOTri60 :: (Real u, Floating u, FromPtSize u) 
            => BPathF u -> AGraphic2 u (BPath u)
arrowOTri60 pathF = AGraphic2 id (mkDF_tipR pathF otri60) (\_ -> pathF)

arrowOTri45 :: (Real u, Floating u, FromPtSize u) 
            => BPathF u -> AGraphic2 u (BPath u)
arrowOTri45 pathF = AGraphic2 id (mkDF_tipR pathF otri45) (\_ -> pathF)



mkDF_tipR :: (Real u, Floating u, FromPtSize u) 
          => BPathF u 
          -> (Radian -> DrawingAttr -> GraphicF u)
          -> DrawingAttr -> Point2 u -> Point2 u 
          -> Graphic u 
mkDF_tipR pathF tipF attr p0 p1 = 
    pathGraphic short_path attr . tipF theta attr p1
  where
    sz          = arrowWidth attr
    long_path   = pathF p0 p1
    short_path  = shortenR sz long_path
    theta       = directionR long_path
                     



arrowPerp :: (Real u, Floating u, FromPtSize u) 
          => BPathF u -> AGraphic2 u (BPath u)
arrowPerp pathF = AGraphic2 id df mf
  where
    df attr p0 p1 = let theta = langle p0 p1  in
                    pathGraphic (pathF p0 p1) attr . perp theta attr p1
    mf _    p0 p1 = pathF p0 p1
