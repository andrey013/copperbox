{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Tree
-- Copyright   :  (c) Stephen Tetley 2010-2011
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Note - this module is a kludge whilst I work out a usable API.
-- 
--------------------------------------------------------------------------------

module Wumpus.Tree
  (
  -- * Re-exports
    module Wumpus.Tree.OTMConnectors

  , TreeDirection(..) 
  , tree_direction

  -- * Definitions
  , runTree
  , standardTreeProps

  )
  where

import Wumpus.Tree.Base
import Wumpus.Tree.Draw
import Wumpus.Tree.OTMConnectors

import Wumpus.Basic.Kernel                      -- package: wumpus-basic


import Data.Tree ( Tree )






runTree :: (Real u, Floating u, InterpretUnit u) 
        => TreeProps u a -> Tree (LocImage u a) -> LocGraphic u
runTree props = drawTree props


standardTreeProps :: Fractional u 
                  => u -> u -> OTMAnchorConn u a -> TreeProps u a
standardTreeProps sx sy otm_conn = 
    TreeProps { tp_scale_in_x = sx 
              , tp_scale_in_y = sy
              , tp_multiconn  = otm_conn         
              , tp_direction  = TREE_DOWN
              }  

