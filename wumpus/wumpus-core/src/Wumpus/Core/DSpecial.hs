{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Core.DSpecial
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Versions of common constructors specialized to Double for the
-- Unit type.
--
-- The changes at Version 0.50.0 enabled Wumpus-Core to more 
-- usefully handle different units, but the changes increased the
-- annotation burden on client code.
--
-- This module provides utility constructors specialized to Double 
-- Wumpus-Core\'s default unit.
-- 
--------------------------------------------------------------------------------

module Wumpus.Core.DSpecial
  ( 
   
    dP2
  , dzeroPt
  , dvec

  , dkernchar
  , dkernEscName
  , dkernEscInt

  ) where

import Wumpus.Core.Geometry
import Wumpus.Core.Picture
import Wumpus.Core.PictureInternal

-- | Type (unit) specialized version of the Point2 constructor 'P2'.
--
dP2 :: Double -> Double -> DPoint2
dP2 = P2

-- | Type (unit) specialized version of 'zeroPt'.
--
dzeroPt :: DPoint2
dzeroPt = zeroPt


-- | Type (unit) specialized version of 'vec'.
--
dvec :: Double -> Double -> DVec2
dvec = vec



-- | Type (unit) specialized version of 'kernchar'.
--
dkernchar :: Double -> Char -> KerningChar
dkernchar = kernchar


-- | Type (unit) specialized version of 'kernEscName'.
--
dkernEscName :: Double -> String -> KerningChar
dkernEscName = kernEscName

-- | Type (unit) specialized version of 'kernEscInt'.
--
dkernEscInt :: Double -> Int -> KerningChar
dkernEscInt = kernEscInt
