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

  -- * Pending addition to Wumpus.Core

  -- * Picture transformers
    backgroundFill
  , clipPicture
  , clipToBoundary

  ) where



import Wumpus.Core

--------------------------------------------------------------------------------

-- Pending addition to wumpus-core

--------------------------------------------------------------------------------

-- Picture transformers

-- | Fill the background of a picture (where the backgound area is 
-- given by the bounding box).
backgroundFill :: (Num u, Ord u) => ToPSColour c => c -> Picture u -> Picture u
backgroundFill c p = p `over` rect where
    rect = frame $ fill (toPSColour c) $ vertexPath $ corners $ boundary p


clipPicture :: (Num u, Ord u) => BoundingBox u -> Picture u -> Picture u
clipPicture bb p = clip (vertexPath $ corners bb) p

clipToBoundary :: (Num u, Ord u) => Picture u -> Picture u
clipToBoundary p = clip (vertexPath $ corners $ boundary p) p

