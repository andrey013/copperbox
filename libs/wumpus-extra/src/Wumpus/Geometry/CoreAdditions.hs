{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Geometry.CoreAdditions
-- Copyright   :  (c) Stephen Tetley 2009-2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC with TypeFamilies and more
--
-- Additions due to be added to Wumpus.Core
--
--------------------------------------------------------------------------------


module Wumpus.Geometry.CoreAdditions
  (

  -- * Pending additions to Wumpus.Core
    dashOffset
  , evenDashes
  , solid

  ) where


import Wumpus.Core




-- Pending addition to wumpus-core (or maybe not).

-- Should these produce a DashPattern or a StrokeAttr?

evenDashes :: Int -> DashPattern 
evenDashes n = Dash 0 [(n,n)]

dashOffset :: Int -> DashPattern -> DashPattern
dashOffset _ Solid       = Solid
dashOffset n (Dash _ xs) = Dash n xs


-- solid creating a StrokeAttr rather than a DashPattern seems 
-- uncontroversial ...
solid :: StrokeAttr
solid = DashPattern Solid
