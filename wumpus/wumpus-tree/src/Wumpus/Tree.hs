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
  , runTreeLoc

  -- * Definitions
  , standardTreeProps

  )
  where

import Wumpus.Tree.Base
import Wumpus.Tree.DrawLoc
import Wumpus.Tree.OTMConnectors






standardTreeProps :: Fractional u 
                  => u -> u -> OTMAnchorConn u a -> TreeProps u a
standardTreeProps sx sy otm_conn = 
    TreeProps { tp_sibling_distance = sx 
              , tp_level_distance   = sy
              , tp_multiconn        = otm_conn         
              , tp_direction        = TREE_DOWN
              }  

