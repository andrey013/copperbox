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
-- Line drawing from vertex lists as-per OpenGL...
-- 
--------------------------------------------------------------------------------

module Wumpus.Extra.Lines
  ( 

  -- * Dots
    dots
  , linesUnconnected
  , lineStrip
  , lineLoop
  , triangles
  , triangleStrip
  , triangleFan
  , quads
  , quadStrip 

  ) where



import Wumpus.Core

dots :: (Num u, Ord u) => (Point2 u -> Picture u) -> [Point2 u] -> Picture u
dots f = multi . map f

-- Colours, stroke width, filling... to think about. 



linesUnconnected :: (Num u, Ord u, Stroke t) => t -> [Point2 u] -> Picture u
linesUnconnected t = frameMulti . step where
  step (a:b:xs) = (ostroke t $ vertexPath [a,b]) : step xs
  step _        = []



lineStrip :: (Num u, Ord u, Stroke t) => t -> [Point2 u] -> Picture u
lineStrip t = frame . ostroke t . vertexPath

lineLoop :: (Num u, Ord u, Stroke t) => t -> [Point2 u] -> Picture u
lineLoop t = frame . cstroke t . vertexPath


triangles :: (Num u, Ord u, Stroke t) => t -> [Point2 u] -> Picture u
triangles t = frameMulti . step where
  step (a:b:c:xs) = (cstroke t $ vertexPath [a,b,c]) : step xs
  step _          = []

-- NOTE corners are too sharp...
triangleStrip :: (Num u, Ord u, Stroke t) => t -> [Point2 u] -> Picture u
triangleStrip t = frameMulti . step where
  step (a:b:c:xs) = (cstroke t $ vertexPath [a,b,c]) : step (b:c:xs)
  step _          = []

triangleFan :: (Num u, Ord u, Stroke t) => t -> [Point2 u] -> Picture u
triangleFan _ []     = error "Wumpus.Extra.Lines.triangleFan - empty list" 
triangleFan t (z:zs) = frameMulti $ step zs where
  step (a:b:xs) = (cstroke t $ vertexPath [z,a,b]) : step (b:xs)
  step _        = []



quads :: (Num u, Ord u, Stroke t) => t -> [Point2 u] -> Picture u
quads t = frameMulti . step where
  step (a:b:c:d:xs) = (cstroke t $ vertexPath [a,b,c,d]) : step xs
  step _            = []


quadStrip :: (Num u, Ord u, Stroke t) => t -> [Point2 u] -> Picture u
quadStrip t = frameMulti . step where
  step (a:b:c:d:xs) = (cstroke t $ vertexPath [a,b,d,c]) : step (c:d:xs)
  step _            = []
  
  -- Note flipping d & c when creating vertex path,
  -- and only requeuing c & d
