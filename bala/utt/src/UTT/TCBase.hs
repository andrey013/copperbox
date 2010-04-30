{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  UTT.TCBase
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Type classes
--
--------------------------------------------------------------------------------

module UTT.TCBase
  ( 
  -- * Typeclass for inversion
    Invert(..)

  -- * Multiplication
  , Mult(..)

  ) where


class Invert a where invert :: a -> a



--------------------------------------------------------------------------------

infixl 7 ^*^

class Mult a where
  (^*^) :: a -> a -> a
