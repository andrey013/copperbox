{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Drawing.Connectors.Base
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Connectors...
--
--------------------------------------------------------------------------------

module Wumpus.Drawing.Connectors.Base
  ( 
    promoteConnS

  ) where


import Wumpus.Basic.Kernel                      -- package: wumpus-basic

import Wumpus.Core                              -- package: wumpus-core

import Data.AffineSpace                         -- package: vector-space

-- | Promote a function for souce and dest points to a connector 
-- function accounting for the separator values in the 
-- DrawingContext.
--
-- This should be used instead of @promoteR2@ for functions 
-- building connectors.
--
promoteConnS :: (Real u, Floating u, InterpretUnit u) 
             => (Point2 u -> Point2 u -> CF a) -> CF2 (Point2 u) (Point2 u) a
promoteConnS fn = promoteR2 $ \p0 p1 -> 
    connectorSrcSep >>= \sep0 ->
    connectorDstSep >>= \sep1 ->
    let ang = vdirection $ pvec p0 p1
    in fn (p0 .+^ avec ang sep0) (p1 .-^ avec ang sep1)
   

