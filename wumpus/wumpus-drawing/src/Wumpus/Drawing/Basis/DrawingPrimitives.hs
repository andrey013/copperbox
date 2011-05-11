{-# OPTIONS -Wall #-}


--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Drawing.Basis.DrawingPrimitives
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  highly unstable
-- Portability :  GHC 
--
-- Alternative to the @DrawingPrimitives@ module in Wumpus-Basic.
-- 
-- The drawing primitives here are either slightly higher level or
-- less general (more quirky).
--
-- This module is expected to be imported qualified - other modules
-- (e.g. shapes and paths) are likely to export conflicting names.
--
--------------------------------------------------------------------------------

module Wumpus.Drawing.Basis.DrawingPrimitives
  (
    pivotLine

  )

  where

import Wumpus.Basic.Kernel                      -- package: wumpus-basic
import Wumpus.Core                              -- package: wumpus-core

import Data.AffineSpace                         -- package: vector-space

-- | @pivotLine@ : @ left_length * right_length * incline -> LocGraphic @
--
-- Draw a /pivot/ line. The start point is a pivot along the line, 
-- not the end. The left and right distances are the extension of
-- the line from the pivot. 
--
pivotLine :: (Floating u, InterpretUnit u) => u -> u -> Radian -> LocGraphic u
pivotLine lu ru ang = promoteR1 $ \pt -> 
    straightLine (pt .+^ avec (ang+pi) lu) (pt .+^ avec ang ru)

