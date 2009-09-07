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

import Bala.Duration

import Data.Stream ( Stream, (<:>), head, tail )
import qualified Data.Stream as S

import Data.Stream.Hinze.Stream ( (<<) )
import qualified Data.Stream.Hinze.Stream as HS


import Data.List ( unfoldr )
import Data.Ratio
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


repeatPattern :: Int -> SubsetPattern -> SubsetPattern
repeatPattern n (SubsetPattern t oset) = SubsetPattern (t*n) (fn n oset)
  where 
    fn i st | i > 1     = fn (i-1) $ st `Set.union` Set.map (t*(i-1) +) oset
            | otherwise = st



-- | Print a SubsetPattern in /box notation/.
showBox :: SubsetPattern -> String
showBox (SubsetPattern t st) = unfoldr phi 1 where
  phi n | n > t             = Nothing
  phi n | n `Set.member` st = Just ('X',n+1)
        | otherwise         = Just ('.',n+1)



-- Note - the comparison @acc >= maxn@ uses the accumulated length 
-- of the previous unfold step.
extractBars :: RationalDuration a 
            => Rational -> Rational -> Integer -> Stream a -> [a]
extractBars asis barlen n = unfoldSt phi 0 where
  phi e acc | acc >= maxn = Nothing
            | otherwise   = Just (e,acc + rationalDuration e)
  maxn  = asis + (barlen*(n%1))



-- cannot use @diff@ in Hinze.Stream as repeating the subset 
-- pattern generates a /numerical reset/ in the stream.
pulse :: SubsetPattern -> Stream Rational
pulse (SubsetPattern t st) = S.zipWith ((rat .) . dif) s (1 <:> s) where
  s      = (Set.toList st) << s

  rat a = (1 % fromIntegral t) * fromIntegral a
  dif a b | a > b        = a - b
          | otherwise    = (a+t) - b



unfoldSt :: (a -> st -> Maybe (b,st)) -> st -> Stream a -> [b]
unfoldSt phi st strm = case phi (head strm) st of
  Just (a,st') -> a : unfoldSt phi st' (tail strm)
  Nothing      -> []

rewrite :: [a -> [b]] -> Stream a -> Stream b
rewrite fs = cross (S.cycle  fs) where
  cross funs strm = xs << cross (tail funs) (tail strm) where
                      xs = (head funs) $ head strm
                  

anarewrite :: Int -> [a -> [a]] -> Stream a -> Stream a
anarewrite i funs strm = xs << rewrite funs rest where
  (xs,rest) = S.splitAt i strm
 
