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

  -- * Picture transformers
    backgroundFill
  , background
  , clipPicture
  , clipToBoundary

  ) where



import Wumpus.Core



--------------------------------------------------------------------------------

-- Picture transformers

-- | Fill the background of a picture (where the backgound area is 
-- given by the bounding box).
backgroundFill :: (Fractional u, Ord u, PSColour c) 
               => c -> Picture u -> Picture u
backgroundFill c p = p `over` rect where
    rect = frame $ fill (psColour c) $ vertexPath $ corners $ boundary p

-- | Coloured but otherwise blank picture, bottom left at the 
-- origin.
background :: (Fractional u, Ord u, PSColour c) 
           => c -> u -> u -> Picture u
background c w h = 
  frame $ fill (psColour c) $ vertexPath $ corners $ bbox zeroPt (P2 w h)


clipPicture :: (Fractional u, Ord u) 
            => BoundingBox u -> Picture u -> Picture u
clipPicture bb p = clip (vertexPath $ corners bb) p


clipToBoundary :: (Fractional u, Ord u) 
               => Picture u -> Picture u
clipToBoundary p = clip (vertexPath $ corners $ boundary p) p

