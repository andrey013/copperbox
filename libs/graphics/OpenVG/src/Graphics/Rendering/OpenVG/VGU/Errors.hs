{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenVG.VGU.Errors
-- Copyright   :  (c) Stephen Tetley 2008, 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  unstable
-- Portability :  GHC
--
-- This module corresponds to section 4.1 (Errors) 
-- of the OpenVG 1.0.1 specs.
--
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenVG.VGU.Errors (
   Error(..), ErrorCategory(..), errors
) where

import Graphics.Rendering.OpenVG.VGU.ErrorsInternal (
   Error(..), ErrorCategory(..), getErrors )
   
import Data.StateVar (
   GettableStateVar, makeGettableStateVar )


--------------------------------------------------------------------------------

errors :: GettableStateVar [Error]
errors = makeGettableStateVar getErrors



   
