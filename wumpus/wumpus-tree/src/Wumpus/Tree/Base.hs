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


type OTMAnchorConn u a = TreeDirection -> u -> a -> [a] -> Graphic u


data TreeProps u a = TreeProps
      { tp_sibling_distance :: u
      , tp_level_distance   :: u
      , tp_multiconn        :: OTMAnchorConn u a
      , tp_direction        :: TreeDirection
      }  

type TreePropsF u a = TreeProps u a -> TreeProps u a

data TreeDirection = TREE_UP | TREE_DOWN | TREE_LEFT | TREE_RIGHT
  deriving (Eq,Ord,Show)

tree_direction :: TreeDirection -> TreePropsF u a
tree_direction dir props = props { tp_direction = dir }


getTreeConnector :: (DrawingCtxM m, InterpretUnit u)
                 => TreeProps u a -> m (a -> [a] -> Graphic u)
getTreeConnector (TreeProps { tp_level_distance   = lvl
                            , tp_multiconn        = conn
                            , tp_direction        = dir  }) = 
    uconvertCtx1 lvl >>= \ulvl -> return (conn dir ulvl)

