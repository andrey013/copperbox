{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Precis.CabalFinalize
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
--
--------------------------------------------------------------------------------


module Precis.CabalFinalize
  (
    FinalizeError
  , cabalFinalize

  ) where

import Distribution.Compiler
import Distribution.PackageDescription
import Distribution.PackageDescription.Configuration
import Distribution.System
import Distribution.Version


type FinalizeError = String

cabalFinalize :: Version 
              -> GenericPackageDescription 
              -> Maybe PackageDescription
cabalFinalize v pkg = either (const Nothing) (Just . fst) $ 
    finalizePackageDescription [] (const True) buildPlatform comp [] pkg
  where
    comp = CompilerId buildCompilerFlavor v
