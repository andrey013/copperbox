{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Graphic
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC 
--
-- Graphic type - HList of primitive
--
--------------------------------------------------------------------------------

module Wumpus.Basic.Graphic
  (
  -- * Type aliases
    Graphic  
  , DGraphic

  , GraphicF
  , DGraphicF

  -- * Operations
  , drawGraphic

  ) where



import Wumpus.Core                      -- package: wumpus-core
import Wumpus.Basic.Utils.HList

-- | Note - this representation allows for zero, one or more
-- Primitives to be collected together.
--
type Graphic u = H (Primitive u)

type DGraphic  = Graphic Double

type GraphicF u = Point2 u -> H (Primitive u)

type DGraphicF = GraphicF Double



--------------------------------------------------------------------------------

-- | Note - Pictures cannot be empty whereas Graphics can.
-- Hence this function returns via Maybe.
--
drawGraphic :: (Floating u, Ord u ) => Graphic u -> Maybe (Picture u)
drawGraphic f = post $ f []
  where
    post [] = Nothing
    post xs = Just $ frameMulti $ xs 