{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Orchsyn.Language.Prelude
-- Copyright   :  (c) Stephen Tetley 2012
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  unstable
-- Portability :  GHC
--
-- Re-exports Haskell\'s Prelude hiding identifiers redfined by
-- Orchsyn.
--
-- Acknowledgement - this idea is from Lee Pike\'s Copilot.
--
--------------------------------------------------------------------------------


module Orchsyn.Language.Prelude
  (
   
    module Prelude
  ) where

import Prelude hiding 
  ( 
    (&&), (||), (^)
  )