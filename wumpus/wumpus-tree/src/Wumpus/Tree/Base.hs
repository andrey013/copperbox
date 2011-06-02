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

    OTMConn(..)
  , TreeProps(..)
  , TreePropsF
  , TreeDirection(..)
  , tree_direction

  ) where


import Wumpus.Drawing.Basis.RefTrace            -- package: wumpus-drawing

-- import Wumpus.Basic.Kernel                      -- package: wumpus-basic



newtype OTMConn u a = OTMConn  { 
          getOTMConn :: TreeDirection -> u -> Ref -> [Ref] -> RefGraphic u a }


data TreeProps u a = TreeProps
      { tp_sibling_distance     :: u
      , tp_level_distance       :: u
      , tp_one_to_many_conn     :: OTMConn u a
      , tp_direction            :: TreeDirection
      }  

type TreePropsF u a = TreeProps u a -> TreeProps u a

data TreeDirection = TREE_UP | TREE_DOWN | TREE_LEFT | TREE_RIGHT
  deriving (Eq,Ord,Show)

tree_direction :: TreeDirection -> TreePropsF u a
tree_direction dir props = props { tp_direction = dir }

{-
getTreeConnector :: (DrawingCtxM m, InterpretUnit u)
                 => TreeProps u a -> m (a -> [a] -> Graphic u)
getTreeConnector (TreeProps { tp_level_distance   = lvl
                            , tp_one_to_many_conn = conn
                            , tp_direction        = dir  }) = 
    uconvertCtx1 lvl >>= \ulvl -> return ((getOTMConn conn) dir ulvl)
-}
