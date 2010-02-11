{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Neume.OneTwo
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- c.f. the Maybe type but using digits 1 & 2 rather than 0 & 1.
--
--------------------------------------------------------------------------------

module Neume.OneTwo
  (
    OneTwo(..)
   
  ) where




data OneTwo a b = One  a
                | Two  a b
  deriving (Eq, Ord, Read, Show)

