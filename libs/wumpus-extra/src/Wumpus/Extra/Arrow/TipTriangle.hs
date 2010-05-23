{-# LANGUAGE FlexibleContexts           #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Extra.Arrow.TipTriangle
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC with TypeFamilies and more
--
-- Triangular tips...
--
--------------------------------------------------------------------------------

module Wumpus.Extra.Arrow.TipTriangle
  (

    latexTip

  ) where

import Wumpus.Core

import Wumpus.Extra.Arrow.Base
import Wumpus.Extra.Shape

import Data.AffineSpace


-- For an arrow pointing right @ -> @
--
-- Rotate about tip, after calculating these coords
--
tripoints :: Num u => u -> u -> Point2 u -> (Point2 u, Point2 u)
tripoints left_extend half_height tip = (tip .+^ v1, tip .+^ v2)
  where
    v1   = V2 (-left_extend) half_height 
    v2   = V2 (-left_extend) (-half_height)



-- /latex/ style arrow 

latexTip :: (Floating u, Real u, Ord u) => ArrowTip u
latexTip = ArrowTip (*2) fn 
  where
    fn lw theta end = simpleComposite $ rotateAbout theta end 
                                      $ fill ()           
                                      $ vertexPath [end,a,b]
      where
        (a,b) = tripoints (lw*4) (lw*1.5) end

        -- WARNING - should have a gentle curve...

