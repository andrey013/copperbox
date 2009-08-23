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


import Data.Set ( Set )
import qualified Data.Set as Set

-- | Represent rhythm patterns in /subset notation/. Subset 
-- notation is useful as it readily accommodates rhythms that 
-- don't start on a beat.
data RhythmPattern = RhythmPattern { 
      pulseSet :: (Set Int), 
      timespan :: Int
    }
  deriving (Eq,Show)

-- | Clockwise sequence notation
type CWSN = [Int]


-- Convert to /clockwise sequence notation/. Note the result is
-- paired with an /anacrusis/ indicating the onset of the first 
-- beat. 
toCWSN :: RhythmPattern -> (Int,CWSN)
toCWSN (RhythmPattern s t) = fn (Set.toAscList s) 
  where
    fn []       = (0,[]) -- error ?
    fn (x:xs)   = (x, zipWith (-) (xs++[t]) (x:xs))


-- | Build a RhythmPattern. This function throws a if the subset 
-- list is empty, ot if it it contains indexes outside the range 
-- [0..n-1].
makeRhythmPattern :: [Int] -> Int -> RhythmPattern
makeRhythmPattern [] _  = error "makeRhythmPattern - empty list"
makeRhythmPattern xs n
    | all (>=0) xs && n > maximum xs 
                        = RhythmPattern (Set.fromList xs) n 
    | otherwise         = error "makeRhythmPattern - invalid data"
        


-- | Print a RhythmPattern in /box notation/.
showBox :: RhythmPattern -> String
showBox rp = take (timespan rp) $ post a $ foldr fn "" xs
  where
    (a,xs)     = toCWSN rp
    fn n acc   = ('X' : replicate (n-1) '.') ++ acc
    post n acc = replicate n '.' ++ acc


cycleCWSN :: (Int,CWSN) -> (Int,CWSN)
cycleCWSN (0,xs) = (0,cycle xs)
cycleCWSN (n,xs) = (n,cycle $ xs') where
  xs' = init xs ++ [n+last xs]

{-
-- | Interpret a RhythmPattern. Note the pattern will be cycled to 
-- produce an infinite list.
interpret :: (Int -> e) -> [Int -> e] -> RhythmPattern -> [e]
interpret anaf fs rp = case toCWSN rp of 
  (0,xs) -> zipWith ($) fs (cycle xs)
  (a,xs) -> anaf a : zipWith ($) fs (cycle xs)
-}

-- | Interpret a RhythmPattern. Note the pattern will be cycled to 
-- produce an infinite list.
interpret :: (Int -> e) -> [Int -> (Maybe Int, e)] -> RhythmPattern -> [e]
interpret restf fs rp = case cycleCWSN $ toCWSN rp of 
  (0,xs) -> funnyZip restf fs xs
  (a,xs) -> restf a : funnyZip restf fs xs

funnyZip :: (st -> e) -> [a -> (Maybe st, e)] -> [a] -> [e]
funnyZip _         []     _      = []
funnyZip _         _      []     = []
funnyZip flushStep (f:fs) (x:xs) = case f x of
    (Just st,e) -> e : flushStep st : funnyZip flushStep fs xs
    (Nothing,e) -> e : funnyZip flushStep fs xs


 



 