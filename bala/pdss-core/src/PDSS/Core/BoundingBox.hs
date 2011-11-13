{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  PDSS.Core.BoundingBox
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Bounding Box.
--
--------------------------------------------------------------------------------


module PDSS.Core.BoundingBox
  ( 

    BoundingBox(..)
  , Boundary(..)

  ) where 

import PDSS.Core.InternalTypes

-- | Bounding box of an object - bang, toggle, etc. represented by
--  the lower-left and upper-right corners.
-- 
-- 
data BoundingBox = BBox 
      { ll_corner :: Point
      , ur_corner :: Point 
      }
  deriving (Eq,Ord,Show)



--------------------------------------------------------------------------------
-- Boundary class

-- | Type class extracting the bounding box of an object - 
-- Picture, Path etc.
--
class Boundary t where
  boundary :: t -> BoundingBox