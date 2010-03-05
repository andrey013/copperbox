{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Neume.Core.TreeOps
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Operations on the 'PletTree'.
--
--------------------------------------------------------------------------------

module Neume.Core.TreeOps
  (
  -- * Operations
    pletFold
  , pletAll
  , pletMeasure
  , pletCount

  ) where

import Neume.Core.BeamExtremity
import Neume.Core.Datatypes
import Neume.Core.Duration

import Data.List ( foldl' )
import Data.Ratio


pletFold :: (a -> b -> b) -> (Int -> Int -> b -> b) -> b -> PletTree a -> b
pletFold f _ b (S a)         = f a b
pletFold f g b (Plet p q xs) = foldl' (pletFold f g) (g p q b) $ getNoteList xs


pletAll :: (a -> Bool) -> PletTree a -> Bool
pletAll test (S a)            = test a
pletAll test (Plet _ _ notes) = step (getNoteList notes) where
   step []                      = True
   step (p:ps) | pletAll test p = step ps
   step _                       = False


--------------------------------------------------------------------------------
-- Measuring the plet-tree, and tuplet stack


-- | The measure of a \single\ or a \plet tree\ - plet trees are
-- considered indivisable so it is not a problem to sum them.
--
pletMeasure :: (Measurement a ~ DurationMeasure, NumMeasured a) 
            => PletTree a -> DurationMeasure
pletMeasure = snd . pletFold  phi chi (tempty,0) where
  phi a      (stk,acc) = (stk, acc + sf stk * nmeasure a)
  chi p q    (stk,acc) = (pushT p q stk,acc) 


type TStack = [Rational]


-- scale factor
sf :: TStack -> Rational
sf []     = (1%1)
sf (x:xs) = x * sf xs

tempty :: TStack
tempty = []

pushT :: Int -> Int -> TStack -> TStack
pushT p q stk = s : stk where
  s = (fromIntegral q) % (fromIntegral p)   -- note - q%p 

--------------------------------------------------------------------------------

-- | The number of items in a PletTree 
--
-- NOTE - need the pred to test e.g. grace notes, which 
-- shouldn\'t be counted.
--
-- Is this a suitable case for another Type Class?
--
pletCount :: (a -> Bool) -> PletTree a -> Int
pletCount test = pletFold phi chi 0 where
  phi a   n  | test a    = n+1
             | otherwise = n
  chi _ _ n              = n

