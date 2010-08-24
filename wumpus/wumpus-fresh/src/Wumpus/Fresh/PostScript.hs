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
-- Fresh PostScript.
--
--------------------------------------------------------------------------------

module Wumpus.Fresh.PostScript
  where

import Wumpus.Fresh.FormatCombinators
import Wumpus.Fresh.FreshIR
import Wumpus.Fresh.Geometry
import Wumpus.Fresh.PostScriptDoc
import Wumpus.Fresh.Utils

-- import Data.Time


-- This will need to become monadic to handle /colour delta/.
--
fillEllipse :: PSUnit u => RGB255 -> u -> Point2 u -> Doc
fillEllipse rgb radius (P2 x y) = 
    vcat [ ps_newpath,  ps_arc x y radius 0 360, ps_closepath, ps_fill ]

strokeEllipse :: PSUnit u => RGB255 -> u -> Point2 u -> Doc
strokeEllipse rgb radius (P2 x y) = 
    vcat [ ps_newpath,  ps_arc x y radius 0 360, ps_closepath, ps_stroke ]




