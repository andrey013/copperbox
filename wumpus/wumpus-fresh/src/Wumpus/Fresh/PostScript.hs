{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Fresh.PostScript
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Fresh SVG.
--
--------------------------------------------------------------------------------

module Wumpus.Fresh.PostScript
  where

import Wumpus.Fresh.BoundingBox
import Wumpus.Fresh.Utils

import Text.PrettyPrint.Leijen                  -- package: wl-pprint


epsPrologue :: PSUnit u => BoundingBox u -> Doc
epsPrologue bb = undefined