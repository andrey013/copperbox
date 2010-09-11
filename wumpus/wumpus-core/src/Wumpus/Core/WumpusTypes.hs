{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Core.WumpusTypes
-- Copyright   :  (c) Stephen Tetley 2009-2010
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
-- 
-- Note - XLink only exports the NoLink constructors.

module Wumpus.Core.WumpusTypes
  (

  -- * Picture types
    Picture
  , DPicture
  , PrimElement
  , DPrimElement
  , FontCtx
  , Primitive
  , DPrimitive
  , XLink
  , PrimPath
  , DPrimPath
  , PrimPathSegment
  , DPrimPathSegment
  , PrimLabel
  , DPrimLabel
  , KerningChar
  , DKerningChar

  -- * Drawing styles
  , PathProps       
  , LabelProps
  , EllipseProps



  -- * Transformations on PrimElements
  , rotatePrim
  , scalePrim
  , uniformScalePrim
  , translatePrim


  -- * Printable unit for PostScript
  , PSUnit(..)

  ) where


import Wumpus.Core.PictureInternal
import Wumpus.Core.Utils ( PSUnit(..) )

