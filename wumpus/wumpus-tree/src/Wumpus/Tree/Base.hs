{-# LANGUAGE FlexibleContexts           #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Tree.Base
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Common types, ...
--
--------------------------------------------------------------------------------

module Wumpus.Tree.Base
  (
    TreeDrawing
  , DTreeDrawing
  , CoordTree
  
  , TreeNode

  ) where

import Wumpus.Basic.Kernel                      -- package: wumpus-basic
import Wumpus.Drawing.Dots.AnchorDots

import Wumpus.Core                              -- package: wumpus-core

import Data.Tree

-- | A rendered tree - alias for for @Picture Double@ in 
-- Wumpus-Core.
--
type TreeDrawing u = Drawing u

type DTreeDrawing = TreeDrawing Double


-- | Tree annotated with positions.
--
type CoordTree u a = Tree (Point2 u, a)



type TreeNode u = DotLocImage u



