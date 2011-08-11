{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Kernel.Drawing.Basis
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  highly unstable
-- Portability :  GHC 
--
-- User state class for Drawing monads.
--
--------------------------------------------------------------------------------

module Wumpus.Basic.Kernel.Drawing.Basis
  (

    UState
  , UserStateM(..)

  , LocationM(..)
  , CursorM(..)
  , BranchCursorM(..)
  , hmoveby
  , vmoveby

  ) where

import Wumpus.Basic.Kernel.Base.BaseDefs
import Wumpus.Basic.Kernel.Objects.Basis
import Wumpus.Basic.Kernel.Objects.LocImage

import Wumpus.Core                              -- package: wumpus-core

import Control.Applicative

type family UState m :: *


class (Applicative m, Monad m) => UserStateM (m :: * -> *) where
  getState    :: st ~ UState m  => m st
  setState    :: st ~ UState m  => st -> m ()
  updateState :: st ~ UState m  => (st -> st) -> m ()
   



class Monad m => LocationM (m :: * -> *) where
  location  :: u ~ DUnit (m ()) => m (Point2 u)


-- | 'insertl' analogue to Writer monad @tell@.
--
class LocationM m => CursorM (m :: * -> *) where
  insertl   :: u ~ DUnit (m ()) => LocImage u a -> m a
  insertl_  :: u ~ DUnit (m ()) => LocImage u a -> m (UNil u)
  
  moveby    :: u ~ DUnit (m ()) => Vec2 u -> m ()


  insertl_ = insertl . ignoreAns 



-- | Add operations for branching at the current point.
-- 
-- Not all drawings that support tracing support branching. For
-- instance Paths can be built by tracing but they always need 
-- a cumulative progression of /next point/ they cannot resrt to 
-- the start point and go in a differnt direction.
-- 
class CursorM m => BranchCursorM (m :: * -> *) where
  -- | Branch is like @local@ in the Reader monad.
  branchCursor    :: m a -> m a



--------------------------------------------------------------------------------
-- Derived operations


-- | Move the /cursor/ horizontally.
--
hmoveby :: (CursorM m, Num u, u ~ DUnit (m ())) => u -> m ()
hmoveby dx = moveby (hvec dx)

-- | Move the /cursor/ vertically.
--
vmoveby :: (CursorM m, Num u, u ~ DUnit (m ())) => u -> m ()
vmoveby dx = moveby (vvec dx)
