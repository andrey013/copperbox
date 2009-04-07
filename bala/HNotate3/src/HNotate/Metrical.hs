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

import HNotate.Cardinal
import HNotate.Duration
import HNotate.NamedElements ( eighth )
import HNotate.StructuralDatatypes
import HNotate.Utils


import Data.Sequence ( Seq, ViewL (..), ( <| ), viewl, empty )
import qualified Data.Sequence as S

data TieStatus = Tied | NotTied
  deriving (Eq,Show)


makeSection :: (Temporal a, Spacer a) =>
    Duration -> [Duration] -> [[Duration]] -> Seq a -> Section a
makeSection anacrusis bar_lengths meter_patterns note_list = 
    Section . map Single $ partitionAndBeam ds_bar dss_beam note_list   
  where    
    ds_bar    = reduceStk anacrusis bar_lengths
    dss_beam  = case meter_patterns of 
                  xs:xss -> (reduceStk anacrusis xs) : xss
                  _      -> meter_patterns    

             
partitionAndBeam :: Temporal a => [Duration] -> [[Duration]] -> Seq a -> [Bar a]
partitionAndBeam ds_bar dss_beam notelist = 
    zipWith fn (divideToBars ds_bar notelist) dss_beam 
  where    
    fn (_,[])          _  = Bar []
    fn (Tied, n:notes) xs = TiedBar n (beam stk notes) where 
                                stk = reduceStk (duration n) xs 
    fn (_, notes)      xs = Bar $ beam xs notes

    
-- split a list of notes into bars of given duration 
divideToBars :: Temporal a => [Duration] -> Seq a -> [(TieStatus, [a])] 
divideToBars ds ns = fst $ anaMap fn (NotTied,ns) ds where
  fn d (tie,sa) | S.null sa = Nothing
                | otherwise = let (ls, opt_split, rest) = fitTill d sa in
                              maybe (Just ((tie,ls), (NotTied, rest)))
                              (\split_note -> Just ((tie,ls), (Tied, split_note <| rest)))
                              opt_split

  
fitTill :: Temporal a => Duration -> Seq a -> ([a], Maybe a, Seq a)
fitTill d0 se = step d0 (viewl se) where
    step _ EmptyL     = ([],Nothing,empty) 
    step d (a :< sa)  = let d' = duration a in 
                    case d' `compare` d of
                        EQ -> ([a],Nothing,sa)
                        LT -> (a:ls,pivot,rs) where
                                (ls,pivot,rs) = fitTill (d-d') sa
                        GT -> ([swapDuration d a], Just $ swapDuration (d'-d) a, sa)
                         
--------------------------------------------------------------------------------
-- beaming

-- beam splitting as an unfold. 
-- For syntactical clarity we use a 'double unfoldr' rather than 
-- tuple the two elements of state together.
-- The state is (1) the stack of durations for each beam group, 
-- and (2) the input stream of notes.
 
beam :: Temporal a => [Duration] -> [a] -> [Cardinal a]
beam = unfoldr2 fn where
    fn _          []          = Nothing             -- notes exhausted
    fn []         (x:xs)      = Just (Single x, [],xs)   -- duration exhausted, return singletons
    fn dstk       (x:xs)  
        | duration x > eighth = Just (Single x, reduceStk (duration x) dstk, xs)
    
    fn dstk@(d:_) xs          = let (count,l,r) = beamGroup1 d xs in 
                                Just (fromList l, reduceStk count dstk, r)


-- cannot reduce stack by negative amounts...
reduceStk :: Duration -> [Duration] -> [Duration]
reduceStk d stk     | d <= 0    = stk       
reduceStk _ []                  = []
reduceStk d (x:xs)  | d == x    = xs
                    | d > x     = reduceStk (d-x) xs
                    | otherwise = (d-x):xs 
                      
                      
-- beamGroup1 is always called on non-empty list

beamGroup1 :: Temporal a => Duration -> [a] -> (Duration,[a],[a])
beamGroup1 d xs = if c >= d then (c,[l],rs) else (c+c', l:ls', rs')  
  where
    (c,l,rs)     = consume1 xs
    (c',ls',rs') = consumes (d-c) rs

-- beaming must always consume at least one note...
-- (and consume1 always called on non-empty list)
consume1 :: Temporal a => [a] -> (Duration,a,[a])
consume1 (x:xs) = (duration x,x,xs)
consume1 []     = error $ "consume1 - unreachable"


consumes :: Temporal a => Duration -> [a] -> (Duration,[a],[a])
consumes d ys = step 0 ys where
    step c []       = (c,[],[])
    step c (x:xs)   = case d `compare` (c + duration x) of
                        EQ -> (d, [x],xs) 
                        LT -> (c, [],xs)
                        GT -> (c', x:ls,rs) where 
                                 (c',ls,rs) = step (c + duration x) xs     



       