{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Drawing.Shapes
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

module Wumpus.Drawing.Shapes
  ( 
    module Wumpus.Drawing.Shapes.Base
  , module Wumpus.Drawing.Shapes.Coordinate
  , module Wumpus.Drawing.Shapes.Derived

  ) where

import Wumpus.Drawing.Shapes.Base
import Wumpus.Drawing.Shapes.Coordinate
import Wumpus.Drawing.Shapes.Derived hiding ( mkRectangle )
