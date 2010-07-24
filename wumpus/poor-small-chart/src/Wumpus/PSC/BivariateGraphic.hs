{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.PSC.BivariateGraphic
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC with TypeFamilies and more
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

import Wumpus.Core                              -- package: wumpus-core
import Wumpus.Basic.Graphic                     -- package: wumpus-basic


type BivariateGraphic ux uy       = Bivariate ux uy -> DGraphic


rectangleBorder :: Stroke t => t -> BivariateGraphic ux uy
rectangleBorder t = \bv -> wrapG $ cstroke t $ 
    rectanglePath (borderWidth bv) (borderHeight bv) (borderOrigin bv)