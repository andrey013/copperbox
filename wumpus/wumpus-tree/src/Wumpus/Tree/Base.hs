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

  , OTMAnchorConn
  , TreeProps(..)
  , TreePropsF
  , TreeDirection(..)
  , tree_direction
  , getTreeConnector

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




type OTMAnchorConn u a = TreeDirection -> u -> a -> [a] -> Graphic u


data TreeProps u a = TreeProps
      { tp_scale      :: UW -> UW -> Query (Vec2 u)
      , tp_multiconn  :: OTMAnchorConn u a
      , tp_direction  :: TreeDirection
      }  

type TreePropsF u a = TreeProps u a -> TreeProps u a

data TreeDirection = TREE_UP | TREE_DOWN | TREE_LEFT | TREE_RIGHT
  deriving (Eq,Ord,Show)

tree_direction :: TreeDirection -> TreePropsF u a
tree_direction dir props = props { tp_direction = dir }


getTreeConnector :: TreeProps u a -> Query (a -> [a] -> Graphic u)
getTreeConnector (TreeProps { tp_scale     = scaleQ
                            , tp_multiconn = conn
                            , tp_direction = dir    }) = 
    scaleQ 1 1 >>= \(V2 x y) -> 
    if dir == TREE_UP || dir == TREE_DOWN 
       then return (conn dir y) else return (conn dir x)



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


