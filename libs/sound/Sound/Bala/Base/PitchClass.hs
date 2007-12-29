{-# OPTIONS_GHC -XFlexibleInstances #-}

module Sound.Bala.Base.PitchClass where

import Sound.Bala.Base.PitchRep

import Numeric (showInt)

newtype PC = PC {unPC :: Int}

type PCSet = [PC]

pitchClass i | i >= 0 && i <= 11 = PC i
             | otherwise         = PC $ i `mod` 12

instance Show PC where
  showsPrec _ (PC i)         
      | i > 11 || i < 0 = showString "Error"
      | i == 11         = showChar 'E'
      | i == 10         = showChar 'T'
      | otherwise       = showInt i

class PitchClass a where pc :: a -> PC

instance PitchClass Pitch where
  pc (Pitch n a _ _) = pitchClass $ base n + step a
    where
      base C = 0
      base D = 2
      base E = 4
      base F = 5
      base G = 7
      base A = 9
      base B = 11
      
      step Nat        = 0
      step Sharp      = 1
      step SharpSharp = 2
      step Flat       = -1
      step FlatFlat   = -2
      step (Sharpi i) = i
      step (Flati i)  = (0-i)
  
class Transpose a where transpose :: Int -> a -> a

instance Transpose PC where
  transpose i (PC n) = PC $ (i + n) `mod` 12

instance Transpose [PC] where
  transpose i ps = map (transpose i) ps
   