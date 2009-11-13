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
--  , clipPicture
--  , clipPictureToPath
--  , clipToBoundary

  ) where



import Wumpus.Core



-- Picture transformers

backgroundFill :: (Num u, Ord u) => ToPSColour c => c -> Picture u -> Picture u
backgroundFill c p = p `over` rect where
    rect = frame $ fill (toPSColour c) $ vertexPath $ corners $ boundary p


{-

clipPicture :: BoundingBox u -> Picture u -> Picture u
clipPicture _ _ = undefined

clipPictureToPath :: Path u -> Picture u -> Picture u
clipPictureToPath _ _ = undefined

clipToBoundary :: Picture u -> Picture u
clipToBoundary _ = undefined

-}


