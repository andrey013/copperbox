{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Core.PitchClass
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Pitch class 
--
--------------------------------------------------------------------------------

module Bala.Core.PitchClass 
  ( 
  -- * Datatypes
    PitchClass

  -- * Operations  
  , makePitchClass
  , transpose
  , inverti
  , primeform

  ) where

import Bala.Core.Invert
import Bala.Core.Modulo

import Data.Set hiding ( map )
import qualified Data.Set as Set

newtype PitchClass = PitchClass { getPitchClass :: Set Z12 }
  deriving (Eq)

instance Show PitchClass where
  showsPrec p = showsPrec p . toList . getPitchClass

pcmap :: (Z12 -> Z12) -> PitchClass -> PitchClass
pcmap f = PitchClass . Set.map f . getPitchClass

makePitchClass :: Modulo12 a => [a] -> PitchClass
makePitchClass = PitchClass . fromList . fmap toZ12

transpose :: Z12 -> PitchClass -> PitchClass 
transpose i = pcmap (+i)

instance Invert PitchClass where
  invert = pcmap negate

-- | aka invert and transpose
inverti :: Z12 -> PitchClass -> PitchClass
inverti i = pcmap ((i+) . negate)
  

primeform :: PitchClass -> [Int]
primeform = minspan . rots .  map fromZ12 . toList . getPitchClass   
  where

    minspan (xs:xss) = snd $ foldr best (extdist xs,xs) xss
    minspan _        = error "primeform on an empty PitchClass"
    best :: [Int] -> (Int,[Int]) -> (Int,[Int])
    best xs (spy,ys) = let spx = extdist xs in 
                         case spx `compare` spy of
                           LT -> (spx,xs)
                           GT -> (spy,ys)
                           EQ -> (spx, bestdelta xs ys)

    bestdelta :: [Int] -> [Int] -> [Int]
    bestdelta xs ys = case diff xs `compare` diff ys of
                        LT -> xs
                        _  -> ys
       
    diff xs         = zipWith (-) (tail xs) xs

    extdist (x:xs)  = last xs - x
    extdist _       = error "primeform on an empty PitchClass"

    rots xs         = take (length xs) $ iterate rot1 xs
    rot1 (x:xs)     = xs ++ [12+x]
    rot1 _          = error "primefrom on an empty PitchClass"

{-

class Transpose a where
  transpose :: a -> a -> a
  invert    :: a -> a -> a

instance Transpose Z12 where
  transpose a b = b + a
  invert a b = (negate b) + a

type Triad a = (a,a,a)

parallel :: (Transpose a, Num a) => (a,a,a) -> (a,a,a)
parallel (y1,y2,y3) = (f y1, f y2, f y3) where f = invert (y1+y3)

-- Leading tone exchange
ltExch :: (Transpose a, Num a) => (a,a,a) -> (a,a,a)
ltExch (y1,y2,y3) = (f y1, f y2, f y3) where f = invert (y2+y3)

relative :: (Transpose a, Num a) => (a,a,a) -> (a,a,a)
relative (y1,y2,y3) = (f y1, f y2, f y3) where f = invert (y1+y2)

-}

