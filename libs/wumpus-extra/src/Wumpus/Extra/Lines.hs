{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Extra.Lines
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  experimental
-- Portability :  GHC only
--
-- Line drawing from vertex lists as-per OpenGL and Processing...
-- 
--------------------------------------------------------------------------------

module Wumpus.Extra.Lines
  ( 

  -- * Dots
    dots
  , linesUnconnected
  , lineStrip
  ) where



import Wumpus.Core

dots :: (Num u, Ord u) => (Point2 u -> Picture u) -> [Point2 u] -> Picture u
dots f = multi . map f


linesUnconnected :: [Point2 u] -> [Path u]
linesUnconnected (a:b:xs) = vertexPath [a,b] : linesUnconnected xs
linesUnconnected _        = []

lineStrip :: [Point2 u] -> Path u
lineStrip = vertexPath