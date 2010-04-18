{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Core.Invert
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Invert type class
--
--------------------------------------------------------------------------------

module Bala.Core.Invert 
  ( 
  -- * Invert type class
    Invert(..)

  ) where


class Invert a where invert :: a -> a



