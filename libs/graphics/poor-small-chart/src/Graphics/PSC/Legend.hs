{-# LANGUAGE NamedFieldPuns             #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.PSC.Legend
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC - NamedFieldPuns
--
-- Axes / grids
--
--------------------------------------------------------------------------------

module Graphics.PSC.Legend
  where

import Graphics.PSC.Core
import Graphics.PSC.DrawingUtils
import Graphics.PSC.Utils

import Wumpus.Core                      -- package: wumpus-core

-- import Data.AffineSpace                 -- package: vector-space 



data LegendConfig = LegendConfig
      { legend_label_cfg      :: LabelConfig
      , legend_borderline     :: LineConfig
      } 
