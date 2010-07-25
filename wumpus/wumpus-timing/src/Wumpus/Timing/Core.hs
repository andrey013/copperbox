{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Timing.Core
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC 
--
-- Core types, functions...
--
--------------------------------------------------------------------------------

module Wumpus.Timing.Core
  (
  -- * Types
    TimingDiagram


  , Coordinate

  , LineWidth
  , SignalHeight

  ) where



import Wumpus.Core                      -- package: wumpus-core


type TimingDiagram = DPicture


type Coordinate = Point2 Int


type LineWidth = Double

-- | SignalHeight is synonymous with FontSize
--
type SignalHeight = Int



