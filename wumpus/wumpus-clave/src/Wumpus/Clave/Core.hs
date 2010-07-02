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


  , LineWidth
  , BoxHeight

  ) where



import Wumpus.Core                      -- package: wumpus-core

type BoxDiagram = DPicture


type LineWidth = Double

-- | BoxHeight is synonymous with FontSize
--
type BoxHeight = Int



