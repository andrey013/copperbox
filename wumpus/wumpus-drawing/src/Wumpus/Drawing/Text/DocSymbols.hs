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

-- More to follow...

