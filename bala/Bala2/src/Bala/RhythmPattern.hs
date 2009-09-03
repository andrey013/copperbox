{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.RhythmPattern
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Rhythm represention
--
--------------------------------------------------------------------------------

module Bala.RhythmPattern where


import Data.Stream ( Stream, (<:>), head, tail )
import qualified Data.Stream as S

import Data.Stream.Hinze.Stream ( (<<) )
import qualified Data.Stream.Hinze.Stream as HS


import Data.List ( unfoldr )
import Data.Set ( Set )
import qualified Data.Set as Set

import Prelude hiding (head, tail)
import qualified Prelude as Pre



-- | Represent rhythm patterns in /subset notation/. Subset 
-- notation is useful as it readily accommodates rhythms that 
-- don't start on a beat.
data SubsetPattern = SubsetPattern { 
      timespan :: Int,
      pulseSet :: (Set Int)
    }
  deriving (Eq,Show)


-- | Build a SubsetPattern. This function throws a if the subset 
-- list is empty, ot if it it contains indexes outside the range 
-- [0..n-1].
makeSubsetPattern :: Int -> [Int] -> SubsetPattern
makeSubsetPattern _ [] = error "makeSubsetPattern - empty list"
makeSubsetPattern n xs 
    | all (>=0) xs && n >= maximum xs 
                        = SubsetPattern n (Set.fromList xs)
    | otherwise         = error "makeSubsetPattern - invalid data"



-- | Print a SubsetPattern in /box notation/.
showBox :: SubsetPattern -> String
showBox (SubsetPattern t st) = unfoldr phi 1 where
  phi n | n > t             = Nothing
  phi n | n `Set.member` st = Just ('X',n+1)
        | otherwise         = Just ('.',n+1)

-- cannot use @diff@ in Hinze.Stream as repeating the subset 
-- pattern generates a /numerical reset/ in the stream.
pulse :: SubsetPattern -> Stream Int
pulse (SubsetPattern t st) = mydiff strm where
  strm      = (Set.toList st) << strm  
  mydiff s  = S.zipWith op s (1 <:> s)

  op a b | a > b        = a - b
         | otherwise    = (a+t) - b


data E a = E a | Tied a 
  deriving (Eq,Show)

metricalPartition :: (Num a, Ord a) => a -> Stream a -> Stream (E a)
metricalPartition sz str = step 0 (head str) (tail str) where 
  step i hd rest 
    | i+hd == sz = (E hd) <:> step 0 (head rest) (tail rest)  
    | i+hd >  sz = (E $ sz-i ) <:> (Tied $ i+hd-sz)
                               <:> step (i+hd-sz) (head rest) (tail rest)
    | otherwise  = (E hd) <:> step (i+hd) (head rest) (tail rest)


metricalPartition' :: (Num a, Ord a) => a -> Stream a -> Stream (E a)
metricalPartition' sz = unwind fn 0 where
  fn a i | a+i == sz = (Left (E a),0)
         | a+i > sz  = (Right [E $ sz-i, Tied $ i+a-sz], i+a-sz)
         | otherwise = (Left (E a),a+i)





-- buffered spigot
bspigot :: (st -> Maybe ([b], st)) -> (st -> a -> st) -> st -> Stream a -> Stream b
bspigot prod cons st s = case prod st of
    Just (bs,st') -> bs << bspigot prod cons st' s
    Nothing       -> bspigot prod cons (cons st (head s)) (tail s)


-- this is really an unfoldMap but it can produce 1 or more 
-- results at each step (actually it can produce zero - Right []).

unwind :: (a -> st -> (Either b [b],st)) -> st -> Stream a -> Stream b
unwind f st s = case f (head s) st of
  (Left x,st')   -> x <:> unwind f st' (tail s)
  (Right xs,st') -> xs << unwind f st' (tail s)

 
