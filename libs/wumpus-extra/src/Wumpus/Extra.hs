{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Extra
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Common interface to Wumpus.Extra...
--
-- Neither @Wumpus.Extra.SVGColours@ nor @Wumpus.Extra.X11Colours@ 
-- is imported. Although there are no name clashes between these 
-- modules the advice is to choose one and import it explicitly.
--
--------------------------------------------------------------------------------


module Wumpus.Extra
  ( 
    module Wumpus.Extra.Arrow
  , module Wumpus.Extra.Dots
  , module Wumpus.Extra.Drawing
  , module Wumpus.Extra.Grid
  , module Wumpus.Extra.SafeFonts
  ) where

import Wumpus.Extra.Arrow
import Wumpus.Extra.Dots
import Wumpus.Extra.Drawing
import Wumpus.Extra.Grid
import Wumpus.Extra.SafeFonts

