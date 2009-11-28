{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Extra.Lines
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC with TypeFamilies and more
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

  -- 
  , bend


  ) where



import Wumpus.Core

import Data.AffineSpace
import Data.VectorSpace
dots :: (Fractional u, Ord u) 
     => (Point2 u -> Picture u) -> [Point2 u] -> Picture u
dots f = multi . map f

-- Colours, stroke width, filling... to think about. 



linesUnconnected :: (Fractional u, Ord u, Stroke t) 
                 => t -> [Point2 u] -> Picture u
linesUnconnected t = frameMulti . step where
  step (a:b:xs) = (ostroke t $ vertexPath [a,b]) : step xs
  step _        = []



lineStrip :: (Fractional u, Ord u, Stroke t) 
          => t -> [Point2 u] -> Picture u
lineStrip t = frame . ostroke t . vertexPath

lineLoop :: (Fractional u, Ord u, Stroke t) 
         => t -> [Point2 u] -> Picture u
lineLoop t = frame . cstroke t . vertexPath


triangles :: (Fractional u, Ord u, Stroke t) 
          => t -> [Point2 u] -> Picture u
triangles t = frameMulti . step where
  step (a:b:c:xs) = (cstroke t $ vertexPath [a,b,c]) : step xs
  step _          = []

-- NOTE corners are too sharp...
triangleStrip :: (Fractional u, Ord u, Stroke t) 
              => t -> [Point2 u] -> Picture u
triangleStrip t = frameMulti . step where
  step (a:b:c:xs) = (cstroke t $ vertexPath [a,b,c]) : step (b:c:xs)
  step _          = []

triangleFan :: (Fractional u, Ord u, Stroke t) 
            => t -> [Point2 u] -> Picture u
triangleFan _ []     = error "Wumpus.Extra.Lines.triangleFan - empty list" 
triangleFan t (z:zs) = frameMulti $ step zs where
  step (a:b:xs) = (cstroke t $ vertexPath [z,a,b]) : step (b:xs)
  step _        = []



quads :: (Fractional u, Ord u, Stroke t) 
      => t -> [Point2 u] -> Picture u
quads t = frameMulti . step where
  step (a:b:c:d:xs) = (cstroke t $ vertexPath [a,b,c,d]) : step xs
  step _            = []


quadStrip :: (Fractional u, Ord u, Stroke t) 
          => t -> [Point2 u] -> Picture u
quadStrip t = frameMulti . step where
  step (a:b:c:d:xs) = (cstroke t $ vertexPath [a,b,d,c]) : step (c:d:xs)
  step _            = []
  
  -- Note flipping d & c when creating vertex path,
  -- and only requeuing c & d


--------------------------------------------------------------------------------

bend :: (Floating u, AffineSpace (Point2 u), InnerSpace vec,
          vec ~ Diff (Point2 u), Scalar vec ~ u)
     => Radian -> Radian -> Point2 u -> Point2 u -> Path u
bend oang iang u v = path u [curveTo a b v]
  where
    half_dist = 0.5 * distance u v 
    a         = u .+^ avec oang half_dist 
    b         = v .-^ avec iang (-half_dist)

