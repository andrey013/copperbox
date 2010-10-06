{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.PSC.BivariateGraphic
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Candidates for moving to Wumpus-Basic
-- 
--------------------------------------------------------------------------------

module Wumpus.PSC.BivariateGraphic
  (
  
    BivariateGraphic

  , rectangleBorder

  ) where

import Wumpus.PSC.Bivariate

import Wumpus.Basic.Graphic                     -- package: wumpus-basic

import Control.Applicative

type BivariateGraphic ux uy       = Bivariate ux uy -> DGraphic


rectangleBorder :: BivariateGraphic ux uy
rectangleBorder = strokedRectangle <$> borderWidth <*> borderHeight <*> borderOrigin

