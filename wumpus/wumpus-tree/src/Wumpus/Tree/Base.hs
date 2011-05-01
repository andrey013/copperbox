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

    OTMAnchorConn
  , TreeProps(..)
  , TreePropsF
  , TreeDirection(..)
  , tree_direction
  , getTreeConnector

  ) where



import Wumpus.Basic.Kernel                      -- package: wumpus-basic
import Wumpus.Drawing.Dots.AnchorDots

import Wumpus.Core                              -- package: wumpus-core

import Data.Tree





type OTMAnchorConn u a = TreeDirection -> u -> a -> [a] -> Graphic u


data TreeProps u a = TreeProps
      { tp_scale_in_x :: u
      , tp_scale_in_y :: u
      , tp_multiconn  :: OTMAnchorConn u a
      , tp_direction  :: TreeDirection
      }  

type TreePropsF u a = TreeProps u a -> TreeProps u a

data TreeDirection = TREE_UP | TREE_DOWN | TREE_LEFT | TREE_RIGHT
  deriving (Eq,Ord,Show)

tree_direction :: TreeDirection -> TreePropsF u a
tree_direction dir props = props { tp_direction = dir }


getTreeConnector :: InterpretUnit u 
                 => TreeProps u a -> Query (a -> [a] -> Graphic u)
getTreeConnector (TreeProps { tp_scale_in_x   = sx
                            , tp_scale_in_y   = sy
                            , tp_multiconn    = conn
                            , tp_direction = dir    }) = 
    uconvertCtxF (V2 sx sy) >>= \(V2 x y) -> 
    if dir == TREE_UP || dir == TREE_DOWN 
       then return (conn dir y) else return (conn dir x)


