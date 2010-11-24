{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Shapes
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Shim module for Shapes.
-- 
--------------------------------------------------------------------------------

module Wumpus.Basic.Shapes
  ( 
    module Wumpus.Basic.Shapes.Base
  , module Wumpus.Basic.Shapes.Coordinate
  , module Wumpus.Basic.Shapes.Derived

  ) where

import Wumpus.Basic.Shapes.Base
import Wumpus.Basic.Shapes.Coordinate
import Wumpus.Basic.Shapes.Derived hiding ( mkRectangle )
