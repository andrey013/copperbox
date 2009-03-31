{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  HNotate.Metrical
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined
--
-- Bar splitting and beam grouping.
--
--------------------------------------------------------------------------------



module HNotate.Metrical where

import HNotate.Duration
import HNotate.Utils


data Bar a = Bar [a] | TiedBar a [a]
  deriving (Show)

data TieStatus = Tied | NotTied
  deriving (Eq,Show)

-- split a list of notes into bars of given duration 
metricalSplit :: RhythmicValue a => [Duration] -> [a] -> [Bar a] 
metricalSplit ds ns = fst $ anaMap fn (NotTied,ns) ds where
  fn _ (_,[])     = Nothing
  fn d (tie,xs)   = let (ls, opt_split, rs) = fitTill d xs in
                    maybe (Just (bar tie ls, (NotTied,rs)))
                          (\split_note -> Just (bar tie ls, (Tied, split_note:rs)))
                          opt_split
  
  bar Tied (x:xs) = TiedBar x xs
  bar NotTied xs  = Bar xs
  bar Tied []     = error "metricalSplit - unreachable"      
  
  
fitTill :: RhythmicValue a => Duration -> [a] -> ([a], Maybe a, [a])
fitTill _ []      = ([],Nothing,[]) 
fitTill d (x:xs)  = let d' = duration x in 
                    case d' `compare` d of
                        EQ -> ([x],Nothing,xs)
                        LT -> (x:ls,pivot,rs) where
                                (ls,pivot,rs) = fitTill (d-d') xs
                        GT -> ([swapDuration d x], Just $ swapDuration (d'-d) x, xs)
                         


-- beam splitting as an unfold. 
-- For syntactical clarity we use a 'double unfoldr' rather than 
-- tuple the two elements of state together.
-- The state is (1) the stack of durations for each beam group, 
-- and (2) the input stream of notes.
 
beam :: RhythmicValue a => [Duration] -> [a] -> [[a]]
beam = unfoldr2 fn where
    fn _          []          = Nothing             -- notes exhausted
    fn []         (x:xs)      = Just ([x], [],xs)   -- duration exhausted, return singletons
    fn dstk       (x:xs)  
        | duration x > eighth = Just ([x], reduceStk (duration x) dstk, xs)
    
    fn dstk@(d:_) xs          = let (count,l,r) = beamGroup1 d xs in 
                                Just (l, reduceStk count dstk, r)

reduceStk :: Duration -> [Duration] -> [Duration]       
reduceStk _ []                  = []
reduceStk d (x:xs)  | d == x    = xs
                    | d > x     = reduceStk (d-x) xs
                    | otherwise = (d-x):xs 
                      
                      
-- beamGroup1 is always called on non-empty list

beamGroup1 :: RhythmicValue a => Duration -> [a] -> (Duration,[a],[a])
beamGroup1 d xs = if c >= d then (c,[l],rs) else (c+c', l:ls', rs')  
  where
    (c,l,rs)     = consume1 xs
    (c',ls',rs') = consumes (d-c) rs

-- beaming must always consume at least one note...
-- (and consume1 always called on non-empty list)
consume1 :: RhythmicValue a => [a] -> (Duration,a,[a])
consume1 (x:xs) = (duration x,x,xs)
consume1 []     = error $ "consume1 - unreachable"


consumes :: RhythmicValue a => Duration -> [a] -> (Duration,[a],[a])
consumes d ys = step 0 ys where
    step c []       = (c,[],[])
    step c (x:xs)   = case d `compare` (c + duration x) of
                        EQ -> (d, [x],xs) 
                        LT -> (c, [],xs)
                        GT -> (c', x:ls,rs) where 
                                 (c',ls,rs) = step (c + duration x) xs     



 
  