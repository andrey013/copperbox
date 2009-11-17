{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Extra.Drawing
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  experimental
-- Portability :  GHC only
--
-- Picture transformers ...
-- 
--------------------------------------------------------------------------------

module Wumpus.Extra.Drawing 
  ( 

  -- * Pending addition to Wumpus.Core.Picture
    frameMulti
 
 , reflectXPlane  
 , reflectYPlane

  -- * Picture transformers
  , backgroundFill
  , clipPicture
  , clipToBoundary

  ) where



import Wumpus.Core

--------------------------------------------------------------------------------

-- Core.Picture
frameMulti :: (Num u, Ord u) => [Primitive u] -> Picture u
frameMulti = multi . map frame

-- Core.AffineTrans

reflectXPlane :: (Num u, Scale t, Translate t, u ~ DUnit t) 
              => Point2 u -> t -> t
reflectXPlane (P2 x y) = translate x y . scale (-1) 1 . translate (-x) (-y)

reflectYPlane :: (Num u, Scale t, Translate t, u ~ DUnit t) 
              => Point2 u -> t -> t
reflectYPlane (P2 x y) = translate x y . scale 1 (-1) . translate (-x) (-y)

--------------------------------------------------------------------------------

-- Picture transformers

backgroundFill :: (Num u, Ord u) => ToPSColour c => c -> Picture u -> Picture u
backgroundFill c p = p `over` rect where
    rect = frame $ fill (toPSColour c) $ vertexPath $ corners $ boundary p


clipPicture :: (Num u, Ord u) => BoundingBox u -> Picture u -> Picture u
clipPicture bb p = clip (vertexPath $ corners bb) p

clipToBoundary :: (Num u, Ord u) => Picture u -> Picture u
clipToBoundary p = clip (vertexPath $ corners $ boundary p) p




