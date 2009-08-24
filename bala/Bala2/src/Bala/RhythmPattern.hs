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

module Bala.RhythmPattern  where

import Data.Stream.Hinze.Stream 

import Data.Set ( Set )
import qualified Data.Set as Set

import Prelude hiding (head, tail)
import qualified Prelude as Pre

-- Rhythmic Pulse
data Pulse = N Int | R Int
  deriving (Eq,Show)

-- Represent rhythm patterns as a list of pulses. This is more 
-- useful than say subset patterns or box notation, because it
-- contains more information (length of the pulses - rather than
-- just their onset).
newtype RhythmPattern = RhythmPattern { getPulseList :: [Pulse] }




-- | Represent rhythm patterns in /subset notation/. Subset 
-- notation is useful as it readily accommodates rhythms that 
-- don't start on a beat.
data SubsetPattern = SubsetPattern { 
      pulseSet :: (Set Int), 
      timespan :: Int
    }
  deriving (Eq,Show)

-- | Clockwise sequence notation
type CWSN = [Int]


sumRP :: RhythmPattern -> Int
sumRP = foldr fn 0 . getPulseList where
  fn (N i) n = n+i
  fn (R i) n = n+i

toSubsetPattern :: RhythmPattern -> SubsetPattern
toSubsetPattern rp@(RhythmPattern xs) = makeSubsetPattern ys t where
  t  = sumRP rp
  ys = snd $ foldr fn (t,[]) xs
  fn (N i) (n,acc) = let a = n-i in (a,a:acc) 
  fn (R i) (n,acc) = (n-i,acc)


-- | Does the rhythm pattern start on an on-beat?
onBeat :: SubsetPattern -> Bool
onBeat (SubsetPattern s _) = Set.member 0 s

-- Convert to /clockwise sequence notation/. Note the result is
-- paired with an /anacrusis/ indicating the onset of the first 
-- beat. 
toCWSN :: SubsetPattern -> (Int,CWSN)
toCWSN (SubsetPattern s t) = fn (Set.toAscList s) 
  where
    fn []       = (0,[]) -- error ?
    fn (x:xs)   = (x, zipWith (-) (xs++[t]) (x:xs))


-- | Build a SubsetPattern. This function throws a if the subset 
-- list is empty, ot if it it contains indexes outside the range 
-- [0..n-1].
makeSubsetPattern :: [Int] -> Int -> SubsetPattern
makeSubsetPattern [] _  = error "makeSubsetPattern - empty list"
makeSubsetPattern xs n
    | all (>=0) xs && n > maximum xs 
                        = SubsetPattern (Set.fromList xs) n 
    | otherwise         = error "makeSubsetPattern - invalid data"
        


-- | Print a SubsetPattern in /box notation/.
showBox :: SubsetPattern -> String
showBox rp = take (timespan rp) $ post a $ foldr fn "" xs
  where
    (a,xs)     = toCWSN rp
    fn n acc   = ('X' : replicate (n-1) '.') ++ acc
    post n acc = replicate n '.' ++ acc


{-
-- | Interpret a SubsetPattern. Note the pattern will be cycled to 
-- produce an infinite list.
interpret :: (Int -> e) -> [Int -> e] -> SubsetPattern -> [e]
interpret anaf fs rp = case toCWSN rp of 
  (0,xs) -> zipWith ($) fs (cycle xs)
  (a,xs) -> anaf a : zipWith ($) fs (cycle xs)
-}

-- | Interpret a SubsetPattern. Note the pattern will be cycled to 
-- produce an infinite list.
interpret :: (Int -> e) -> [Int -> (Maybe Int, e)] -> SubsetPattern -> [e]
interpret restf fs rp = case cycleCWSN $ toCWSN rp of 
  (0,xs) -> funnyZip restf fs xs
  (a,xs) -> restf a : funnyZip restf fs xs

funnyZip :: (st -> e) -> [a -> (Maybe st, e)] -> [a] -> [e]
funnyZip _         []     _      = []
funnyZip _         _      []     = []
funnyZip flushStep (f:fs) (x:xs) = case f x of
    (Just st,e) -> e : flushStep st : funnyZip flushStep fs xs
    (Nothing,e) -> e : funnyZip flushStep fs xs


cycleCWSN :: (Int,CWSN) -> (Int,CWSN)
cycleCWSN (0,xs) = (0,cycle xs)
cycleCWSN (n,xs) = (n,cycle $ xs') where
  xs' = init xs ++ [n+last xs]


cross :: Stream Int -> Stream Int -> Stream (Either Int Int)
cross as bs = if a >= b then Right b <: cross (tail as) (tail bs)
                        else Right a <: Left (b-a) <: cross (tail as) (tail bs)
  where
   a = head as; b = head bs


-- | Turn a stream into a pair of length to the first onset, and
-- a stream of onset deltas.
toStream' :: SubsetPattern -> (Int,Stream Int)
toStream' p@(SubsetPattern s t) 
    | onBeat p  = (0,st)
    | otherwise = (Pre.head ls,st) 
  where
    ls = Set.toAscList s
    st = (onsetDiff t ls) << st

onsetDiff :: Num a => a -> [a] -> [a]
onsetDiff _ []     = []
onsetDiff t (x:xs) = zipWith (-) (xs++[x+t]) (x:xs)





toStream :: RhythmPattern -> Stream Pulse
toStream (RhythmPattern xs) = st where st = xs << st



takeNBars :: Int -> Int -> Stream Pulse -> [Pulse]
takeNBars n bar_len = step (n*bar_len) where
  step a s = let sh = head s
                 sz = case sh of (N i) -> i; (R i) -> i
             in if (sz>a) then []
                          else sh : step (a-sz) (tail s)

