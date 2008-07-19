{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Format.Output.OutputAbc
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  empty data declarations, multi-parameter typeclasses
--
-- Output Abc. 
-- Similar to Text.XHTML in the Hierarchical Libraries, but with some extra
-- typefulness due to the Abc phantom type.
--
--------------------------------------------------------------------------------

module Bala.Format.Output.OutputAbc
  ( module Bala.Format.Output.AbcInternals
  , module Bala.Format.Output.OutputBase
  ) where

import Bala.Format.Output.AbcInternals
import Bala.Format.Output.OutputBase
