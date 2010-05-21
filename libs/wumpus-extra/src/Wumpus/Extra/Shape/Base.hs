{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Extra.Shape.Base
-- Copyright   :  (c) Stephen Tetley 2009-2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC with TypeFamilies and more
--
-- Common core for shapes (anchors...)
-- 
--------------------------------------------------------------------------------

module Wumpus.Extra.Shape.Base
  ( 

  -- * Current transformation matrix
    CTM
  , translateCTM
  , scaleCTM
  , rotateCTM
  , rotateAboutCTM

  -- * Anchors
  , AnchorCenter(..)
  , AnchorCardinal(..)


  ) where

import Wumpus.Core hiding ( CTM )


--------------------------------------------------------------------------------
-- Shapes will generally include a translation matrix...

type CTM u = Matrix3'3 u

translateCTM :: Num u => u -> u -> CTM u -> CTM u
translateCTM x y m = translationMatrix x y * m

scaleCTM :: Num u => u -> u -> CTM u -> CTM u
scaleCTM x y m = scalingMatrix x y * m

rotateCTM       :: (Floating u, Real u) => Radian -> CTM u -> CTM u
rotateCTM r m   = rotationMatrix r * m

rotateAboutCTM  :: (Floating u, Real u) => Radian -> Point2 u -> CTM u -> CTM u
rotateAboutCTM r pt m = originatedRotationMatrix r pt * m





--------------------------------------------------------------------------------
-- Anchors

class AnchorCenter a where
  center :: DUnit a ~ u => a -> Point2 u

class AnchorCardinal a where
  north :: DUnit a ~ u => a -> Point2 u
  south :: DUnit a ~ u => a -> Point2 u
  east  :: DUnit a ~ u => a -> Point2 u
  west  :: DUnit a ~ u => a -> Point2 u

  northeast :: DUnit a ~ u => a -> Point2 u
  southeast :: DUnit a ~ u => a -> Point2 u
  southwest :: DUnit a ~ u => a -> Point2 u
  northwest :: DUnit a ~ u => a -> Point2 u

