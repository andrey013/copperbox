{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Anchors
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC with TypeFamilies and more
--
-- Anchor points on \"shapes\".
--
-- ** WARNING ** this module is highly experimental, and may 
-- change significantly or even be dropped from future revisions.
-- 
--------------------------------------------------------------------------------

module Wumpus.Basic.Anchors
  ( 

  -- * Anchors
    AnchorCenter(..)
  , AnchorCardinal(..)
  , AnchorCardinal2(..)
  , AnchorText(..)
  , AnchorAngle(..)

  ) where

import Wumpus.Core                      -- package: wumpus-core


class AnchorCenter t where
  center :: DUnit t ~ u => t -> Point2 u

class AnchorCardinal t where
  north :: DUnit t ~ u => t -> Point2 u
  south :: DUnit t ~ u => t -> Point2 u
  east  :: DUnit t ~ u => t -> Point2 u
  west  :: DUnit t ~ u => t -> Point2 u

class AnchorCardinal2 t where
  northeast :: DUnit t ~ u => t -> Point2 u
  southeast :: DUnit t ~ u => t -> Point2 u
  southwest :: DUnit t ~ u => t -> Point2 u
  northwest :: DUnit t ~ u => t -> Point2 u

-- | 'textAnchor' is the Bottom left corner 
-- on the baseline.
--
class AnchorText t where
  textAnchor :: DUnit t ~ u => t -> Point2 u


-- | Anchor on a border that can be identified with and angle.
--
class AnchorAngle t where
  anchorAngle :: DUnit t ~ u => Radian -> t -> Point2 u


-- Note - AnchorAngle.
-- Obviously, circles would support this. In TikZ rectangles do...
