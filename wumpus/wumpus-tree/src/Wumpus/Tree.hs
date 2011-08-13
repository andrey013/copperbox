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
    module Wumpus.Tree.Base
  , module Wumpus.Tree.DrawLoc
  , module Wumpus.Tree.OTMConnectors
  , module Wumpus.Tree.VersionNumber

  -- * Definitions
  , standardTreeProps

  )
  where

import Wumpus.Tree.Base
import Wumpus.Tree.DrawLoc
import Wumpus.Tree.OTMConnectors
import Wumpus.Tree.VersionNumber






standardTreeProps :: Fractional u 
                  => u -> u -> TreeProps u
standardTreeProps sx sy = 
    TreeProps { tp_sibling_distance = sx 
              , tp_level_distance   = sy
              , tp_direction        = TREE_DOWN
              }  

