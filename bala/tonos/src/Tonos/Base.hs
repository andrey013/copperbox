{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Tonos.Base
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Type classes...
--
--------------------------------------------------------------------------------

module Tonos.Base
  ( 
  -- * Typeclass for inversion
    Invert(..)

  -- * Multiplication
  , Mult(..)

  -- * Distance
  , Distance(..)

  -- * semitone count
  , Semitones(..)


  ) where


class Invert a where invert :: a -> a



--------------------------------------------------------------------------------

infixl 7 ^*^

class Mult a where
  (^*^) :: a -> a -> a



-- Implementations should follow the conditions for metric spaces
--
-- distance a b >= 0
-- distance a b == distance b a
--
class Distance a where
  distance :: a -> a -> Int



class Semitones a where
  semitones :: a -> Int