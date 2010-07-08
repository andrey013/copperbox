{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Core
-- Copyright   :  (c) Stephen Tetley 2009-2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  unstable 
-- Portability :  GHC with TypeFamilies and more
--
-- Common interface to Wumpus.Core.
--
-- This module re-exports types and functions from:
--
-- * "Wumpus.Core.AffineTrans"
--
-- * "Wumpus.Core.BoundingBox"
--
-- * "Wumpus.Core.Colour"
--
-- * "Wumpus.Core.FontSize"
--
-- * "Wumpus.Core.Geometry"
--
-- * "Wumpus.Core.GraphicsState"
--
-- * "Wumpus.Core.OutputPostScript"
--
-- * "Wumpus.Core.OutputSVG"
--
-- * "Wumpus.Core.Picture"
--
-- * "Wumpus.Core.PictureLanguage"
--
-- * "Wumpus.Core.TextEncoder"
--
-- * "Wumpus.Core.VersionNumber"
--
-- * "Wumpus.Core.WumpusTypes"
--
-- Named colours ( black, white etc.) are hidden from 
-- "Wumpus.Core.Colour" to avoid collisions with modules that
-- define colour sets (e.g. all the SVG colours). 
--
-- 
--------------------------------------------------------------------------------


module Wumpus.Core
  (
    module Wumpus.Core.AffineTrans 
  , module Wumpus.Core.BoundingBox
  , module Wumpus.Core.Colour
  , module Wumpus.Core.FontSize
  , module Wumpus.Core.Geometry
  , module Wumpus.Core.GraphicsState
  , module Wumpus.Core.OutputPostScript
  , module Wumpus.Core.OutputSVG
  , module Wumpus.Core.Picture
  , module Wumpus.Core.TextEncoder
  , module Wumpus.Core.VersionNumber
  , module Wumpus.Core.WumpusTypes

  ) where

import Wumpus.Core.AffineTrans
import Wumpus.Core.BoundingBox
import Wumpus.Core.Colour hiding ( black, white, red, green, blue )
import Wumpus.Core.FontSize
import Wumpus.Core.Geometry
import Wumpus.Core.GraphicsState
import Wumpus.Core.OutputPostScript
import Wumpus.Core.OutputSVG
import Wumpus.Core.Picture
import Wumpus.Core.TextEncoder
import Wumpus.Core.VersionNumber
import Wumpus.Core.WumpusTypes

