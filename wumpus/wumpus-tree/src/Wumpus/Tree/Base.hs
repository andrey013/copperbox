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
  , OTMAnchorConn
  , TreeProps(..)
  , TreeDirection(..)

  , runTreeMonad

  , drawConn

  ) where


import Wumpus.Basic.Kernel                      -- package: wumpus-basic

import Wumpus.Core                              -- package: wumpus-core

import Control.Applicative
import Data.Monoid

data TreeMonad node u a = TreeMonad { 
        getTreeMonad :: TreeProps node u -> LocDrawing u a }

type instance DUnit (TreeMonad node u a) = u

type TreeGraphic u = TreeMonad u (UNil u)

type OTMAnchorConn node u = TreeDirection -> u -> node -> [node] -> Graphic u



-- Design note - as the TreeMonad will be used internally there
-- doesn\'t seem to be a need for user state.

data TreeProps node u = TreeProps
      { tp_sibling_distance     :: u
      , tp_level_distance       :: u
      , tp_direction            :: TreeDirection
      , tp_otm_connector        :: OTMAnchorConn node u
      }  

type instance DUnit (TreeProps node u) = u


data TreeDirection = TREE_UP | TREE_DOWN | TREE_LEFT | TREE_RIGHT
  deriving (Eq,Ord,Show)




-- Note - not entirely sure TreeDrawing is a LocTrace, it will
-- certainly need branching if it is.

-- Functor

instance Functor (TreeMonad node u) where
  fmap f ma = TreeMonad $ \env -> fmap f $ getTreeMonad ma env 

-- Applicative
 
instance Applicative (TreeMonad node u) where
  pure a    = TreeMonad $ \_   -> pure a
  mf <*> ma = TreeMonad $ \env -> 
                getTreeMonad mf env <*> getTreeMonad ma env


-- Monad

instance Monad (TreeMonad node u) where
  return a  = TreeMonad $ \_   -> return a
  ma >>= k  = TreeMonad $ \env -> 
                getTreeMonad ma env >>= \ans -> getTreeMonad (k ans) env

-- Monoid 

instance Monoid a => Monoid (TreeMonad node u a) where
  mempty          = TreeMonad $ \_   -> mempty
  ma `mappend` mb = TreeMonad $ \env -> 
                      getTreeMonad ma env `mappend` getTreeMonad mb env


-- DrawingCtxM

instance DrawingCtxM (TreeMonad node u) where
  askDC           = TreeMonad $ \_   -> askDC
  asksDC fn       = TreeMonad $ \_   -> asksDC fn
  localize upd ma = TreeMonad $ \env -> localize upd (getTreeMonad ma env)



instance InterpretUnit u => LocDrawM (TreeMonad node u) where
  inserti  gf       = TreeMonad $ \_ -> inserti gf
  insertli p1 gf    = TreeMonad $ \_ -> insertli p1 gf
  insertci p1 p2 gf = TreeMonad $ \_ -> insertci p1 p2 gf


runTreeMonad :: (Translate a, InterpretUnit u, u ~ DUnit a)
             => TreeMonad node u a -> TreeProps node u -> LocImage u a
runTreeMonad ma props = runLocDrawing $ getTreeMonad ma props



drawConn :: InterpretUnit u 
         => node -> [node] -> TreeMonad node u ()
drawConn start kids = TreeMonad $ \(TreeProps { tp_level_distance = h  
                                              , tp_otm_connector  = conn
                                              , tp_direction      = tdir
                                              }) ->
    let gf = conn tdir h start kids in inserti_ gf

