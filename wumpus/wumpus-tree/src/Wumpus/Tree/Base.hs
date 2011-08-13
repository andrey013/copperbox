{-# LANGUAGE TypeFamilies               #-}
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

    TreeMonad
  , TreeGraphic
  , TreeProps(..)
  , TreeDirection(..)

  , runTreeMonad

  ) where


import Wumpus.Basic.Kernel                      -- package: wumpus-basic

import Wumpus.Core                              -- package: wumpus-core

import Control.Applicative
import Data.Monoid

data TreeMonad u a = TreeMonad { 
        getTreeMonad :: TreeProps u -> LocDrawing u a }

type instance DUnit (TreeMonad u a) = u

type TreeGraphic u = TreeMonad u (UNil u)


-- Design note - as the TreeMonad will be used internally there
-- doesn\'t seem to be a need for user state.

data TreeProps u = TreeProps
      { tp_sibling_distance     :: u
      , tp_level_distance       :: u
      , tp_direction            :: TreeDirection
      }  

type instance DUnit (TreeProps u) = u


data TreeDirection = TREE_UP | TREE_DOWN | TREE_LEFT | TREE_RIGHT
  deriving (Eq,Ord,Show)




-- Note - not entirely sure TreeDrawing is a LocTrace, it will
-- certainly need branching if it is.

-- Functor

instance Functor (TreeMonad u) where
  fmap f ma = TreeMonad $ \env -> fmap f $ getTreeMonad ma env 

-- Applicative
 
instance Applicative (TreeMonad u) where
  pure a    = TreeMonad $ \_   -> pure a
  mf <*> ma = TreeMonad $ \env -> 
                getTreeMonad mf env <*> getTreeMonad ma env


-- Monad

instance Monad (TreeMonad u) where
  return a  = TreeMonad $ \_   -> return a
  ma >>= k  = TreeMonad $ \env -> 
                getTreeMonad ma env >>= \ans -> getTreeMonad (k ans) env

-- Monoid 

instance Monoid a => Monoid (TreeMonad u a) where
  mempty          = TreeMonad $ \_   -> mempty
  ma `mappend` mb = TreeMonad $ \env -> 
                      getTreeMonad ma env `mappend` getTreeMonad mb env


-- DrawingCtxM

instance DrawingCtxM (TreeMonad u) where
  askDC           = TreeMonad $ \_   -> askDC
  asksDC fn       = TreeMonad $ \_   -> asksDC fn
  localize upd ma = TreeMonad $ \env -> localize upd (getTreeMonad ma env)



instance InterpretUnit u => LocDrawM (TreeMonad u) where
  inserti  gf       = TreeMonad $ \_ -> inserti gf
  insertli p1 gf    = TreeMonad $ \_ -> insertli p1 gf
  insertci p1 p2 gf = TreeMonad $ \_ -> insertci p1 p2 gf


runTreeMonad :: InterpretUnit u 
             => TreeMonad u a -> TreeProps u -> LocImage u a
runTreeMonad ma props = runLocDrawing $ getTreeMonad ma props


{-

-- WRONG....

-- | @u@ is level distance.
--
data OTMConn node u = OTMConn  
       { otm_parent_ancr :: TreeDirection -> node -> Point2 u
       , otm_child_ancr  :: TreeDirection -> node -> Point2 u
       , otm_draw        :: [Vec2 u] -> LocGraphic u
       }
       
       
type instance DUnit (OTMConn node u) = u




drawConn :: InterpretUnit u => node -> [node] -> TreeGraphic node u
drawConn start kids = TreeMonad $ \(TreeProps { tp_level_distance   = h  
                                              , tp_one_to_many_conn = conn
                                              , tp_direction        = tdir
                                              }) ->
    let p1 = otm_parent_ancr conn tdir start
        vs = map (pvec p1 . otm_child_ancr conn tdir) kids
        gf = otm_draw conn vs
    in insertli p1 gf

-}