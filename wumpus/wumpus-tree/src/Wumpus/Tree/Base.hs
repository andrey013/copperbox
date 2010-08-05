{-# LANGUAGE FlexibleContexts           #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Tree.Base
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Common types, ...
--
--------------------------------------------------------------------------------

module Wumpus.Tree.Base
  (
    TreePicture
  , CoordTree
  
  , TreeNode

  ) where


import Wumpus.Core                              -- package: wumpus-core
import Wumpus.Basic.AnchorDots                  -- package: wumpus-basic
import Wumpus.Basic.Monads.Drawing
import Wumpus.Basic.Monads.SnocDrawing

import Data.Tree

-- | A rendered tree - alias for for @Picture Double@ in 
-- Wumpus-Core.
--
type TreePicture = Picture Double


-- | Tree annotated with positions.
--
type CoordTree u a = Tree (Point2 u, a)


type TreeNode = MGraphicF (SnocDrawing Double) Double (DotAnchor Double)

