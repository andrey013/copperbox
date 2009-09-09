{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Invert
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Invert type class
--
--------------------------------------------------------------------------------

module Bala.Invert 
  ( 
  -- * Invert type class
    Invert(..)

  ) where


class Invert a where invert :: a -> a



