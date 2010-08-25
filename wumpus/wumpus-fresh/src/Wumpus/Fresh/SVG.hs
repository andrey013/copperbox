{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Fresh.SVG
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

module Wumpus.Fresh.SVG 
  where

import Wumpus.Fresh.FreshIR
import Wumpus.Fresh.SVGDoc
import Wumpus.Fresh.Utils


-- Note - it will be wise to make coordinate remapping and output
-- separate passes (unlike in Wumpus-Core). Then I\'ll at least 
-- be able to debug the remapped Picture.
--

pathSegment :: PSUnit u => PrimPathSegment u -> SvgPathSeg
pathSegment (PLineTo pt)        = svg_path_l pt
pathSegment (PCurveTo p1 p2 p3) = svg_path_c p1 p2 p3