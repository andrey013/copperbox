{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Tree
-- Copyright   :  (c) Stephen Tetley 2010-2011
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Note - this module is a kludge whilst I work out a usable API.
-- 
--------------------------------------------------------------------------------

module Wumpus.Tree
  (
  -- * Re-exports
    TreeDirection(..) 
  , tree_direction
  , familyConn 
  , radialConn

  -- * Definitions
  , runTree
  , standardTreeProps
  , charNode
  , diskNode
  , circleNode
  , textNode

  )
  where

import Wumpus.Tree.Base
import Wumpus.Tree.Design
import Wumpus.Tree.Draw
import Wumpus.Tree.OTMConnectors

import Wumpus.Basic.Kernel                      -- package: wumpus-basic
import Wumpus.Drawing.Dots.AnchorDots

import Wumpus.Core                              -- package: wumpus-core

import Data.VectorSpace                         -- package: vector-space

import Data.Tree ( Tree )



gridScale :: Fractional u => UW -> UW -> Query (Vec2 u)
gridScale uwx uwy = snapmove (1,1) >>= \(V2 x y) ->
                    return $ V2 (x * realToFrac uwx) (y * realToFrac uwy)

scaleFactors :: Fractional u => u -> u -> (UW -> UW -> Query (Vec2 u))
scaleFactors sx sy = \uwx uwy -> 
    return $ V2 (sx * realToFrac uwx) (sy * realToFrac uwy)


runTree :: (Real u, Floating u, InterpretUnit u) 
        => TreeProps u a -> Tree (LocImage u a) -> LocGraphic u
runTree props = 
    drawTree props . orientateCoordTree (tp_direction props) . design


standardTreeProps :: Fractional u 
                  => u -> u -> OTMConnector u a -> TreeProps u a
standardTreeProps sx sy otm_conn = 
    TreeProps { tp_scale      = scaleFactors sx sy
              , tp_multiconn  = otm_conn         
              , tp_direction  = TREE_DOWN
              }  

{-

scaledFamilyTree :: ( Real u, Floating u, InterpretUnit u
                    , CenterAnchor a, CardinalAnchor a 
                    , u ~ DUnit a
                    )
                 => u -> u -> TreeDirection -> Tree (LocImage u a) -> LocGraphic u
scaledFamilyTree sx sy dir tree =  
    drawTree props $ rotTree dir $ design tree
  where
    props = TreeProps { tp_scale      = scaleFactors sx sy
                      , tp_multiconn  = familyConn         
                      , tp_direction  = dir
                      }  

-- Note - these functions won\'t be the proper API.
-- They are just a placeholder whilst I work things out.

scaledTree :: ( Real u, Floating u, InterpretUnit u
              , CenterAnchor a, RadialAnchor a
              , u ~ DUnit a
              )  
           => u -> u -> TreeDirection -> Tree (LocImage u a) -> LocGraphic u
scaledTree sx sy dir tree =  
    drawTree props $ rotTree dir $ design tree
  where
    props = TreeProps { tp_scale      = scaleFactors sx sy
                      , tp_multiconn  = radialConn         
                      , tp_direction  = dir
                      }  

            
-}








--------------------------------------------------------------------------------
-- Drawing functions

-- | Render tree nodes with a single character.
--
--  Useful for rendering @ Data.Tree Char @.
--
charNode :: (Real u, Floating u, InterpretUnit u) 
         => Char -> TreeNode u
charNode = dotChar


-- | Tree nodes with a text label.
--
-- Useful for rendering @ Data.Tree String @.
--
-- Note the width of the label is not accounted for in the 
-- /design/ of the tree. Labels with long texts may overlap.
-- Also, only a single line of text is printed - any text after 
-- the first newline character will be dropped.
--
textNode :: (Real u, Floating u, InterpretUnit u) 
         => String -> TreeNode u
textNode = dotText . uptoNewline
  where
    uptoNewline = takeWhile (/='\n')

-- | Tree nodes with a stroked circle.
--
-- Suitable for printing the shape of a tree, ignoring the data.
--
circleNode :: (Floating u, InterpretUnit u) 
           => RGBi -> (a -> TreeNode u)
circleNode rgb = \_ -> localize (stroke_colour rgb) dotCircle


-- | Tree nodes with a filled circle.
--
-- Suitable for printing the shape of a tree, ignoring the data.
--
diskNode :: (Floating u, InterpretUnit u) 
         => RGBi -> (a -> TreeNode u)
diskNode rgb = \_ -> localize (fill_colour rgb) dotDisk



