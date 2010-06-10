{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Clave.Core
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC 
--
-- Core types, functions...
--
--------------------------------------------------------------------------------

module Wumpus.Clave.Core
  (
  -- * Types
    BoxDiagram

  , Graphic  
  , DGraphic


  , LineWidth
  , BoxHeight

  ) where


import Wumpus.Clave.Utils

import Wumpus.Core                      -- package: wumpus-core


type BoxDiagram = DPicture

-- | Note - this representation allows for zero, one or more
-- Primitives to be collected together.
--
type Graphic u = H (Primitive u)

type DGraphic  = Graphic Double


type LineWidth = Double

-- | BoxHeight is synonymous with FontSize
--
type BoxHeight = Int

