{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Core
-- Copyright   :  (c) Stephen Tetley 2009-2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  unstable 
-- Portability :  GHC
--
-- Common interface to Wumpus.Core.
--
-- This is a /shim/ module re-exporting types and functions from
-- the exposed Wumpus-Core modules. In most cases, importing just 
-- this module should be sufficient to use Wumpus-Core. 
--
-- Named colours ( black, white etc.) are hidden from the module
-- "Wumpus.Core.Colour" to avoid collisions with modules that
-- define colour sets (e.g. all the SVG colours). If named 
-- colours are needed, "Wumpus.Core.Colour" can be imported 
-- directly.
-- 
--------------------------------------------------------------------------------


module Wumpus.Core
  (
    module Wumpus.Core.AffineTrans 
  , module Wumpus.Core.BoundingBox
  , module Wumpus.Core.Colour
  , module Wumpus.Core.FontSize
  , module Wumpus.Core.Geometry
  , module Wumpus.Core.GraphicProps
  , module Wumpus.Core.OutputPostScript
  , module Wumpus.Core.OutputSVG
  , module Wumpus.Core.Picture
  , module Wumpus.Core.PtSize
  , module Wumpus.Core.Text.Base
  , module Wumpus.Core.VersionNumber
  , module Wumpus.Core.WumpusTypes


  ) where

import Wumpus.Core.AffineTrans
import Wumpus.Core.BoundingBox
import Wumpus.Core.Colour hiding ( black, white, red, green, blue, yellow, 
                                   cyan, magenta )
import Wumpus.Core.FontSize
import Wumpus.Core.Geometry
import Wumpus.Core.GraphicProps
import Wumpus.Core.OutputPostScript
import Wumpus.Core.OutputSVG
import Wumpus.Core.Picture
import Wumpus.Core.PtSize
import Wumpus.Core.Text.Base
import Wumpus.Core.VersionNumber
import Wumpus.Core.WumpusTypes

