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
  , familyOTMC
  , blankOTMC
  , radialOTMC
  , splayOTMC

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

import Data.Tree ( Tree )



{-
gridScale :: Fractional u => UW -> UW -> Query (Vec2 u)
gridScale uwx uwy = snapmove (1,1) >>= \(V2 x y) ->
                    return $ V2 (x * realToFrac uwx) (y * realToFrac uwy)
-}


scaleFactors :: Fractional u => u -> u -> (UW -> UW -> Query (Vec2 u))
scaleFactors sx sy = \uwx uwy -> 
    return $ V2 (sx * realToFrac uwx) (sy * realToFrac uwy)


runTree :: (Real u, Floating u, InterpretUnit u) 
        => TreeProps u a -> Tree (LocImage u a) -> LocGraphic u
runTree props = drawTree props


standardTreeProps :: Fractional u 
                  => u -> u -> OTMAnchorConn u a -> TreeProps u a
standardTreeProps sx sy otm_conn = 
    TreeProps { tp_scale_in_x = sx 
              , tp_scale_in_y = sy
              , tp_multiconn  = otm_conn         
              , tp_direction  = TREE_DOWN
              }  


--------------------------------------------------------------------------------
-- Drawing functions

-- | Render tree nodes with a single character.
--
--  Useful for rendering @ Data.Tree Char @.
--
charNode :: (Real u, Floating u, InterpretUnit u) 
         => Char -> DotLocImage u
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
         => String -> DotLocImage u
textNode = dotText . uptoNewline
  where
    uptoNewline = takeWhile (/='\n')

-- | Tree nodes with a stroked circle.
--
-- Suitable for printing the shape of a tree, ignoring the data.
--
circleNode :: (Floating u, InterpretUnit u) 
           => RGBi -> (a -> DotLocImage u)
circleNode rgb = \_ -> localize (stroke_colour rgb) dotCircle


-- | Tree nodes with a filled circle.
--
-- Suitable for printing the shape of a tree, ignoring the data.
--
diskNode :: (Floating u, InterpretUnit u) 
         => RGBi -> (a -> DotLocImage u)
diskNode rgb = \_ -> localize (fill_colour rgb) dotDisk



