{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Drawing.Paths.Illustrate
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Illustrate a path - show the construction of its Bezier curves.
-- 
--------------------------------------------------------------------------------

module Wumpus.Drawing.Paths.Illustrate
  ( 

    illustrate1

  ) where

import Wumpus.Drawing.Paths.Base

import Wumpus.Basic.Kernel

import Wumpus.Core                              -- package: wumpus-core
import Wumpus.Core.Colour
import Data.Monoid


illustrate1 :: (Floating u, InterpretUnit u) => AbsPath u -> Graphic u
illustrate1 path1 = pic1 `mappend` pic2
  where
    pic1  = localize (set_line_width 8 . stroke_colour grey1) $
              renderPath_ OSTROKE path1
    pic2  = localize (set_line_width 1 . stroke_colour black) $
              renderPath_ OSTROKE $ deBezier path1


grey1 :: RGBi
grey1 = RGBi 200 200 200