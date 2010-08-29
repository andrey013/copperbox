{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Fresh.WumpusTypes
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  unstable 
-- Portability :  GHC
--
-- This module re-exports types and functions from 
-- "Wumpus.Core.PictureInternal" but makes them opaque. 

-- 
--------------------------------------------------------------------------------


-- Having this module just re-exporting types should make 
-- the Haddock documentation more cohesive. Modules
-- in wumpus-core should not use this module.


module Wumpus.Fresh.WumpusTypes
  (

  -- * Picture types
    Picture
  , DPicture
  , GSUpdate
  , Primitive
  , DPrimitive
  , PrimPath
  , DPrimPath
  , PrimPathSegment
  , DPrimPathSegment
  , PrimLabel
  , DPrimLabel

  -- * Drawing styles
  , PathProps       
  , LabelProps
  , EllipseProps



  -- * Transformations on Primitives
  , rotatePrimitive
  , scalePrimitive
  , uniformScalePrimitive
  , translatePrimitive


  -- * Printable unit for PostScript
  , PSUnit(..)

  ) where


import Wumpus.Fresh.PictureInternal
import Wumpus.Fresh.Utils ( PSUnit(..) )

