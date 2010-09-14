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
import Wumpus.Basic.Colour.SVGColours
import Wumpus.Basic.Dots
import Wumpus.Basic.Graphic   

import Data.VectorSpace                         -- package: vector-space

import Data.Tree hiding ( drawTree )

-- Don\'t actually need the Turtle of ConsDrawing...

drawTree :: (a -> TreeNode) -> DrawingContext -> CoordTree Double a -> HPrim Double
drawTree drawF ctx tree = execDrawing ctx $ drawTop drawF tree 


drawTop :: (a -> TreeNode) -> CoordTree Double a -> Drawing Double ()
drawTop fn (Node (pt,a) ns) = do 
    ancr <- drawAtImg pt (fn a)
    mapM_ (draw1 fn ancr) ns

draw1 :: (a -> TreeNode) 
      -> DotAnchor Double 
      -> CoordTree Double a 
      -> Drawing Double ()
draw1 fn ancr_from (Node (pt,a) ns) = do
    ancr <- drawAtImg pt (fn a)
    draw $ connector ancr_from ancr
    mapM_ (draw1 fn ancr) ns   


connector :: (Floating u, Real u, InnerSpace (Vec2  u)) 
          => DotAnchor u -> DotAnchor u -> Graphic u
connector a1 a2 = openStroke $ vertexPath [p1,p2]
  where  
    (ang0,ang1)    = anchorAngles (center a1) (center a2)
    p1             = radialAnchor ang0 a1
    p2             = radialAnchor ang1 a2 




anchorAngles :: (Floating u, Real u, InnerSpace (Vec2  u)) 
             => Point2 u -> Point2 u -> (Radian,Radian)
anchorAngles f t = (theta0, theta1)
  where
    conn_v  = pvec f t
    theta0  = direction conn_v
    theta1  = if theta0 < pi then theta0 + pi else theta0 - pi
    

