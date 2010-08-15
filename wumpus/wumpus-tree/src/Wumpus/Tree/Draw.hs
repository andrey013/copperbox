{-# LANGUAGE FlexibleContexts           #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Tree.Draw
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Version number
--
--------------------------------------------------------------------------------

module Wumpus.Tree.Draw 
  (
    drawTree

  ) where

import Wumpus.Tree.Base

import Wumpus.Core                              -- package: wumpus-core

import Wumpus.Basic.Anchors                     -- package: wumpus-basic
import Wumpus.Basic.Dots
import Wumpus.Basic.Graphic   
import Wumpus.Basic.Graphic.DrawingAttr
import Wumpus.Basic.Monads.Drawing
import Wumpus.Basic.Monads.DrawingMonad
import Wumpus.Basic.SVGColours

import Data.VectorSpace                         -- package: vector-space

import Data.Tree hiding ( drawTree )

-- Don\'t actually need the Turtle of ConsDrawing...

drawTree :: (a -> TreeNode) -> DrawingAttr -> CoordTree Double a -> DGraphic
drawTree drawF attr tree = execDrawing attr $ drawTop drawF tree 


drawTop :: (a -> TreeNode) -> CoordTree Double a -> Drawing Double ()
drawTop fn (Node (pt,a) ns) = do 
    ancr <- liftAG (fn a) pt
    mapM_ (draw1 fn ancr) ns

draw1 :: (a -> TreeNode) 
      -> DotAnchor Double 
      -> CoordTree Double a 
      -> Drawing Double ()
draw1 fn ancr_from (Node (pt,a) ns) = do
    ancr <- liftAG (fn a) pt
    connector ancr_from ancr
    mapM_ (draw1 fn ancr) ns   


connector :: (Floating u, Real u, InnerSpace (Vec2  u)) 
          => DotAnchor u -> DotAnchor u -> Drawing u ()
connector afrom ato = trace $ wrapG $ ostroke black $ vertexPath [p0,p1]
   where  
     (ang0,ang1)    = anchorAngles (center afrom) (center ato)
     p0             = radialAnchor ang0 afrom
     p1             = radialAnchor ang1 ato 




anchorAngles :: (Floating u, Real u, InnerSpace (Vec2  u)) 
             => Point2 u -> Point2 u -> (Radian,Radian)
anchorAngles f t = (theta0, theta1)
  where
    conn_v  = pvec f t
    theta0  = direction conn_v
    theta1  = if theta0 < pi then theta0 + pi else theta0 - pi
    

