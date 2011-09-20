{-# OPTIONS -Wall #-}


--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Drawing.Text.DocSymbols
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  highly unstable
-- Portability :  GHC 
--
-- Symbols - redefined Basis.Symbols.
-- 
--------------------------------------------------------------------------------

module Wumpus.Drawing.Text.DocSymbols
  (

    ocircle
  , small_ocircle

  , empty_box

  , left_slice
  , right_slice

  )
  where

import qualified Wumpus.Drawing.Basis.Symbols as S
import Wumpus.Drawing.Text.Base.DocTextZero


import Wumpus.Basic.Kernel                      -- package: wumpus-basic

-- import Wumpus.Core                              -- package: wumpus-core

import Control.Applicative


ocircle :: (Fractional u, InterpretUnit u) => GenDocGraphic st u
ocircle = embedPosObject $ makePosObject qy gf
  where
    qy = (\h -> let hh = 0.5 * h in Orientation hh hh 0 h) <$> capHeight
    gf = capHeight >>= \h -> moveStart (go_up $ 0.5 * h) (S.ocircle $ 0.5 * h)



small_ocircle :: (Fractional u, InterpretUnit u) => GenDocGraphic st u
small_ocircle = embedPosObject $ makePosObject qy gf
  where
    qy = (\h -> let hw = 0.33 * h in Orientation hw hw 0 h) <$> capHeight
    gf = capHeight >>= \h -> moveStart (go_up $ 0.33 * h) (S.ocircle $ 0.25 * h)

empty_box :: (Fractional u, InterpretUnit u) => GenDocGraphic st u
empty_box = embedPosObject $ makePosObject qy gf
  where
    qy = (\h -> let hw = 0.5 * h in Orientation hw hw 0 h) <$> capHeight
    gf = capHeight >>= \h -> moveStart (go_up $ 0.33 * h) (S.empty_box $ 0.66 * h)



left_slice :: (Real u, Floating u, InterpretUnit u) => GenDocGraphic st u
left_slice = embedPosObject $ makePosObject qy gf
  where
    qy = (\h -> let hw = 0.66 * h in Orientation hw hw 0 h) <$> capHeight
    gf = capHeight >>= \h -> moveStart (go_up $ 0.33 * h) (S.left_slice h)

right_slice :: (Real u, Floating u, InterpretUnit u) => GenDocGraphic st u
right_slice = embedPosObject $ makePosObject qy gf
  where
    qy = (\h -> let hw = 0.66 * h in Orientation hw hw 0 h) <$> capHeight
    gf = capHeight >>= \h -> moveStart (go_up $ 0.33 * h) (S.right_slice h)

-- More to follow...

