{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Drawing.Paths
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Shim import module for Paths.
-- 
--------------------------------------------------------------------------------

module Wumpus.Drawing.Paths 
  ( 

    module Wumpus.Drawing.Paths.Base
  , module Wumpus.Drawing.Paths.Connectors
  , module Wumpus.Drawing.Paths.ControlPoints
  , module Wumpus.Drawing.Paths.MonadicConstruction

  ) where

import Wumpus.Drawing.Paths.Base
import Wumpus.Drawing.Paths.Connectors
import Wumpus.Drawing.Paths.ControlPoints
import Wumpus.Drawing.Paths.MonadicConstruction

