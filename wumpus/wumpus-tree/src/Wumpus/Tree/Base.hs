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
  , TreePicture         -- OLD
  , DTreePicture        -- OLD
  , CoordTree
  
  , TreeNode

  , Design(..)

  ) where

import Wumpus.Basic.Kernel                      -- package: wumpus-basic
import Wumpus.Drawing.Dots.AnchorDots

import Wumpus.Core                              -- package: wumpus-core

import Data.Tree


-- | A rendered tree - alias for for @Picture Double@ in 
-- Wumpus-Core.
--
type TreeDrawing u      = TraceDrawing u ()

type DTreeDrawing       = TreePicture Double



-- | A rendered tree - alias for for @Picture Double@ in 
-- Wumpus-Core.
--
-- OLD-HAT.
--
type TreePicture u = CtxPicture u

-- OLD-HAT.
--
type DTreePicture = TreePicture Double


-- | Tree annotated with positions.
--
type CoordTree u a = Tree (Point2 u, a)



type TreeNode u = DotLocImage u


data Design u a = Design 
      { tree_design     :: CoordTree u a
      , tree_bbox       :: BoundingBox u
      }


