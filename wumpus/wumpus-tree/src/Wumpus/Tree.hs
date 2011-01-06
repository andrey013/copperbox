{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Tree
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
--------------------------------------------------------------------------------

module Wumpus.Tree
  (
  -- * The type of rendered trees
    TreePicture
  , DTreePicture        -- re-export.

  , ScaleFactors
  , uniformScaling
  , scaleFactors

  , drawScaledTree
  , drawScaledTree2
  , drawScaledFamilyTree


  -- * Drawing nodes
  , charNode
  , textNode
  , circleNode
  , diskNode

  )
  where

import Wumpus.Tree.Base
import Wumpus.Tree.Design
import Wumpus.Tree.Draw

import Wumpus.Basic.Kernel                      -- package: wumpus-basic
import Wumpus.Drawing.Dots.AnchorDots

import Wumpus.Core                              -- package: wumpus-core

import Data.VectorSpace                         -- package: vector-space

import Data.Tree hiding ( drawTree )



-- | Customize the size of the printed tree.
--
-- A tree is /designed/ with a height of 1 unit between 
-- parent and child nodes.
--
-- The y-scaling factor multiplies the unit height, a scaling 
-- factor of 30 represents 30 /points/.
--
-- In the horizontal, 1 unit is the smallest possible distance 
-- between child nodes.
--
type ScaleFactors u = ScalingContext u Int u

type instance DUnit (ScaleFactors u) = u



-- | Build uniform x- and y-scaling factors, i.e. @ x == y @.
--
uniformScaling :: Num u => u -> ScaleFactors u
uniformScaling u = ScalingContext (\x -> u * x)
                                  (\y -> u * fromIntegral y) 

scaleFactors :: Num u => u -> u -> ScaleFactors u
scaleFactors sx sy = ScalingContext (\x -> sx * x)
                                    (\y -> sy * fromIntegral y)	


-- | 'drawTreePicture' : @ draw_fun * attr * scale_factors * tree -> TreePicture @
--
-- The rendering function.
-- 
-- @draw_fun@ renders the individual nodes. Usually 'charNode', 
-- 'circleNode'
--
-- @attr@ is the font size (translates to node size), stroke 
-- colour, fill colour.
--
-- @scale_factors@ scales the distances between parent and child 
-- (y-scale) and sibling nodes (x-scale).
--
-- @tree@ is the input tree to be rendered.
--
--
drawScaledTree :: (Real u, Floating u, FromPtSize u, InnerSpace (Vec2 u)) 
                => (a -> TreeNode u) 
                -> ScaleFactors u
                -> Tree a 
                -> TreePicture u
drawScaledTree drawF scale_f tree = drawTree drawF $ design scale_f tree


drawScaledTree2 :: (Real u, Floating u, FromPtSize u, InnerSpace (Vec2 u)) 
                => ScaleFactors u
                -> Tree (TreeNode u) 
                -> TreePicture u
drawScaledTree2 scale_f tree = drawTree2 $ design scale_f tree



drawScaledFamilyTree :: (Real u, Floating u, FromPtSize u, InnerSpace (Vec2 u)) 
                     => (a -> TreeNode u) 
                     -> ScaleFactors u
                     -> Tree a 
                     -> TreePicture u
drawScaledFamilyTree drawF scale_f tree = 
    drawFamilyTree drawF $ design scale_f tree

--------------------------------------------------------------------------------
-- Drawing functions

-- | Render tree nodes with a single character.
--
--  Useful for rendering @ Data.Tree Char @.
--
charNode :: (Real u, Floating u, FromPtSize u) 
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
textNode :: (Real u, Floating u, FromPtSize u) 
         => String -> TreeNode u
textNode = dotText . uptoNewline
  where
    uptoNewline = takeWhile (/='\n')

-- | Tree nodes with a stroked circle.
--
-- Suitable for printing the shape of a tree, ignoring the data.
--
circleNode :: (Floating u, FromPtSize u) 
           => RGBi -> (a -> TreeNode u)
circleNode rgb = \_ -> localize (strokeColour rgb) dotCircle


-- | Tree nodes with a filled circle.
--
-- Suitable for printing the shape of a tree, ignoring the data.
--
diskNode :: (Floating u, FromPtSize u) 
         => RGBi -> (a -> TreeNode u)
diskNode rgb = \_ -> localize (fillColour rgb) dotDisk



