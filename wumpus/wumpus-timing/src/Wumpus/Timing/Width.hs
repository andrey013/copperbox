{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Timing.Width
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC 
--
-- Number type with half subscript.
--
--------------------------------------------------------------------------------

module Wumpus.Timing.Width
  (
    Width1
  , Width
  , half

  , width1
  , halfcount

  ) where



data Width1 = H | O
  deriving (Eq,Ord,Show)


data Width = I  Int 
           | IH Int
  deriving (Eq,Ord)

instance Show Width where
  showsPrec _ (I  n)  = shows n
  showsPrec _ (IH n)  = shows n . showString "+1/2"



instance Num Width where
  I  n1 + I  n2     = I  $ n1 + n2
  IH n1 + I  n2     = IH $ n1 + n2
  I  n1 + IH n2     = IH $ n1 + n2
  IH n1 + IH n2     = I  $ n1 + n2 + 1

  I  n1 - I  n2     = I  $ n1 - n2
  IH n1 - I  n2     = IH $ n1 - n2
  I  n1 - IH n2     = IH $ n1 - (n2+1)
  IH n1 - IH n2     = I  $ n1 - n2

  (*) _   _         = error "Width - no multiplication on Width"

  abs _             = error "Width - no abs on Width"
  negate _          = error "Width - no negate on Width"
  signum _          = error "Width - no signum on Width"
  fromInteger n     
        | n >= 0    = I  (fromInteger n)
        | otherwise = error "Width - cannot create a negative Width"
                     

half :: Width 
half = IH 0


width1 :: Width1 -> Width
width1 H = half
width1 O = 1


halfcount :: Width -> Int
halfcount (I n)  = n * 2
halfcount (IH n) = 1 + n * 2
