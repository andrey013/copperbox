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


  -- * Render a Data.Tree to a TreePicture
  , DrawingAttr(..)             -- re-export
  , standardAttr                -- re-export


  , ScaleFactors(..)
  , uniformScaling

  , drawTreePicture

  -- * Output to file
  , writeEPS_TreePicture
  , writeSVG_TreePicture

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

import Wumpus.Basic.AnchorDots                  -- package: wumpus-basic
import Wumpus.Basic.Graphic
import Wumpus.Basic.Monads.Drawing
import Wumpus.Basic.Monads.DrawingCtxClass
import Wumpus.Core                              -- package: wumpus-core

import Data.Maybe
import Data.Tree hiding ( drawTree )

-- | Output a 'TreePicture', generating an EPS file.
--
writeEPS_TreePicture :: FilePath -> TreePicture -> IO ()
writeEPS_TreePicture = writeEPS_latin1

-- | Output a 'TreePicture', generating a SVG file.
--
writeSVG_TreePicture :: FilePath -> TreePicture -> IO ()
writeSVG_TreePicture = writeSVG_latin1


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
data ScaleFactors = ScaleFactors
      { dx_scale :: Double
      , dy_scale :: Double 
      }
  deriving (Eq,Show)

-- | Build uniform x- and y-scaling factors, i.e. @ x == y @.
--
uniformScaling :: Double -> ScaleFactors
uniformScaling u = ScaleFactors u u 

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
drawTreePicture :: (a -> TreeNode) 
                -> DrawingAttr 
                -> ScaleFactors 
                -> Tree a 
                -> TreePicture
drawTreePicture drawF attr sfactors tree = 
    fromMaybe errK $ drawGraphic $ drawTree drawF attr $ design funs tree
  where
    funs = scalingFunctions sfactors

errK :: a
errK = error "treePicture - empty tree drawing." 


scalingFunctions :: ScaleFactors -> (Double -> Double, Int -> Double)
scalingFunctions (ScaleFactors sx sy) = (fx,fy)
  where
    fx d = sx * d
    fy d = sy * fromIntegral d

--------------------------------------------------------------------------------
-- Drawing functions

-- | Render tree nodes with a single character.
--
--  Useful for rendering @ Data.Tree Char @.
--
charNode :: Char -> TreeNode
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
textNode :: String -> TreeNode
textNode = dotText . uptoNewline
  where
    uptoNewline = takeWhile (/='\n')

-- | Tree nodes with a stroked circle.
--
-- Suitable for printing the shape of a tree, ignoring the data.
--
circleNode :: DRGB -> (a -> TreeNode)
circleNode rgb = const fn
  where
    fn = dotCircle `props` (\s -> s { stroke_colour = rgb})


-- | Tree nodes with a filled circle.
--
-- Suitable for printing the shape of a tree, ignoring the data.
--
diskNode :: DRGB -> (a -> TreeNode)
diskNode rgb = const fn
  where
    fn = dotDisk `props` (\s -> s { fill_colour = rgb})



