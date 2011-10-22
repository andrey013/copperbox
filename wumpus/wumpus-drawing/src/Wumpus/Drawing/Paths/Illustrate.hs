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

    path_as_control_box
  , path_with_control_points
    
  ) where

import Wumpus.Drawing.Paths.Base

import Wumpus.Basic.Kernel

import Wumpus.Core                              -- package: wumpus-core
import Wumpus.Core.Colour
import Data.Monoid



grey1 :: RGBi
grey1 = RGBi 200 200 200

-- | Illustrate the control points as a /boxed/ path - Bezier 
-- curves are replaced with straight lines spanning the 
-- control points.
--
path_as_control_box :: (Floating u, InterpretUnit u) => AbsPath u -> Graphic u
path_as_control_box path1 = pic1 `mappend` pic2
  where
    pic1  = localize (set_line_width 8 . stroke_colour grey1) $
              renderPath_ OSTROKE path1
    pic2  = localize (set_line_width 1 . stroke_colour black) $
              renderPath_ OSTROKE $ deBezier path1


path_with_control_points :: (Floating u, InterpretUnit u) => AbsPath u -> Graphic u
path_with_control_points path1 = pic1 `mappend` pic2
  where
    pic1  = localize (fill_colour grey1) $
              mconcat $ map (disk1 `at`) $ pathAllPoints path1

    pic2  = localize (set_line_width 1 . stroke_colour black) $
              renderPath_ OSTROKE path1

    disk1 = dcDisk DRAW_FILL 3

