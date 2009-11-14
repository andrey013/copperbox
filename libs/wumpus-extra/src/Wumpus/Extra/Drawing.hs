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
  -- * Temporary - pending adding to wumpus-core
    blankPicture
  , clip

  -- * Picture transformers
  , backgroundFill
  , clipPicture
  , clipToBoundary

  ) where



import Wumpus.Core

import Wumpus.Core.PictureInternal ( Picture(..) ) -- TO REMOVE...


--------------------------------------------------------------------------------
-- These ones are scheduled to be added to wumpus-core...
blankPicture :: Num u => BoundingBox u -> Picture u
blankPicture bb = PicBlank (ortho zeroPt, bb)

clip :: (Num u, Ord u) => Path u -> Picture u -> Picture u
clip cp p = Clip (ortho zeroPt, boundary cp) cp p




--------------------------------------------------------------------------------

-- Picture transformers

backgroundFill :: (Num u, Ord u) => ToPSColour c => c -> Picture u -> Picture u
backgroundFill c p = p `over` rect where
    rect = frame $ fill (toPSColour c) $ vertexPath $ corners $ boundary p


clipPicture :: (Num u, Ord u) => BoundingBox u -> Picture u -> Picture u
clipPicture bb p = clip (vertexPath $ corners bb) p

clipToBoundary :: (Num u, Ord u) => Picture u -> Picture u
clipToBoundary p = clip (vertexPath $ corners $ boundary p) p




