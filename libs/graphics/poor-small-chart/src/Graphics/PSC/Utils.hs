{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.PSC.Utils
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC with TypeFamilies and more
--
-- Utilities
--
-- WARNING - all out of date?
--
--------------------------------------------------------------------------------

module Graphics.PSC.Utils
  (
    XRange
  , YRange
  , minMax2
  
  ) where


import Wumpus.Core

type XRange = (Double,Double)
type YRange = (Double,Double)


-- None-empty list
minMax2 :: [DPoint2] -> (XRange,YRange)
minMax2 []          = error "minMax2 - empty list"
minMax2 (P2 x0 y0:ps) = foldr fn ((x0,x0),(y0,y0)) ps
  where
    fn (P2 x y) ((xlo,xhi),(ylo,yhi)) = (xrange,yrange)
      where
        xrange = (min x xlo, max x xhi)
        yrange = (min y ylo, max y yhi)

