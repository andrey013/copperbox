{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Tree.Base
-- Copyright   :  (c) Stephen Tetley 2010-2011
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


    UW    
  
  , TreeProps(..)
  , TreeDirection(..)

  , CoordTree
  
  , TreeNode

  , Design(..)

  ) where

import Wumpus.Basic.Kernel                      -- package: wumpus-basic
import Wumpus.Drawing.Dots.AnchorDots

import Wumpus.Core                              -- package: wumpus-core

import Data.Tree

-- | tree unit width...
--
newtype UW = UW { getUW :: Double }
  deriving (Eq,Ord,Num,Floating,Fractional,Real,RealFrac,RealFloat)

instance Show UW where
  showsPrec p d = showsPrec p (getUW d)



-- | A rendered tree - alias for for @Picture Double@ in 
-- Wumpus-Core.
--
-- OLD HAT (?)

-- type TreeDrawing u      = TraceDrawing u ()

-- type DTreeDrawing       = TreeDrawing Double




data TreeProps u a = TreeProps
      { tp_scale      :: UW -> UW -> Query (Vec2 u)
      , tp_multiconn  :: a -> [a] -> Graphic u
      , tp_direction  :: TreeDirection
      }  


data TreeDirection = TREE_UP | TREE_DOWN | TREE_LEFT | TREE_RIGHT
  deriving (Eq,Ord,Show)



-- | Tree annotated with positions.
--
type CoordTree a = Tree (Point2 UW, a)



-- | Tree nodes are currently a /Dot/ from Wumpus-Drawing.
--
-- At some point this should change to allow any object that 
-- supports anchors.
--
type TreeNode u = DotLocImage u


data Design a = Design 
      { tree_design     :: CoordTree a
      , tree_bbox       :: BoundingBox UW
      }


