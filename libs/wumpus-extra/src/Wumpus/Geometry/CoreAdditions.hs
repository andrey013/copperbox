{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Geometry.CoreAdditions
-- Copyright   :  (c) Stephen Tetley 2009
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
  -- * Version
    wumpus_core_version

  -- * Pending additions to Wumpus.Core
  , dashOffset
  , evenDashes
  , solid

  ) where


import Wumpus.Core


wumpus_core_version :: (Int,Int,Int)
wumpus_core_version = (0,13,1)



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
