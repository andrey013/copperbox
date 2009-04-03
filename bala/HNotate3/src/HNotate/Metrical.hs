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
import HNotate.LineTree
import HNotate.NamedElements ( eighth )
import HNotate.NoteList ( collapseTree )
import HNotate.Staff
import HNotate.Utils

import Data.List ( foldl' )
import Data.Sequence ( Seq, ViewL (..), ( <| ), viewl, empty )
import qualified Data.Sequence as S

data TieStatus = Tied | NotTied
  deriving (Eq,Show)



lineTreeToStaffRep :: (Temporal a, Spacer a) =>
    Duration -> [Duration] -> [[Duration]] -> LineTree a -> Staff a
lineTreeToStaffRep anacrusis bar_lengths meter_patterns note_list = 
    Staff . mergeOverlays . map splitAndBeam . map calcOnset 
      $ collapseTree note_list   
  where    
    splitAndBeam (start,xs) = (start, partitionAndBeam ds_bar dss_beam xs) 
    calcOnset = onset anacrusis bar_lengths
    
    ds_bar    = reduceStk anacrusis bar_lengths
    dss_beam  = case meter_patterns of 
                  xs:xss -> (reduceStk anacrusis xs) : xss
                  _      -> meter_patterns    
    
partitionToStaff :: Temporal a => [Duration] -> [[Duration]] -> Seq a -> Staff a
partitionToStaff =  (Staff . map Single) `ooo` partitionAndBeam
             
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



 
--------------------------------------------------------------------------------
-- grouping overlays

-- onset - change the onset time from /collapseTree/ to the bar number
-- Optional prefix a spacer - if the notes start mid-bar. 
onset :: (Temporal a, Spacer a) => 
    Duration -> [Duration] -> (Duration,Seq a) -> (Int,Seq a)
onset anacrusis ds0 (start,notes) = step 1 start stk where
    stk   = reduceStk anacrusis ds0
    
    step bc n _       | n <= 0    = (bc, notes)   
    step bc n (d:ds)  | n < d     = (bc, spacer n <| notes)
                      | otherwise = step (bc+1) (n-d) ds
    step _  _ []                  = error $ "onset - duration stack exhausted"                       

mergeOverlays :: [(Int,[Bar a])] -> [Overlay a]
mergeOverlays (x:xs)  = foldl' zipOverlay (overlay1 x) xs
mergeOverlays []      = []

-- the first /line/ must be transformed to an overlay /by hand/, the other
-- /lines/ can then be zipped into it.
-- @start@ is bar number which starts at 1!
overlay1 :: (Int,[Bar a]) -> [Overlay a]
overlay1 (bar_num,xs) = map Single (replicate (bar_num-1) empty_bar ++ xs) where
    empty_bar = Bar []
    

zipOverlay :: [Overlay a] -> (Int,[Bar a]) -> [Overlay a]
zipOverlay xs (n,ys) 
    | n == 1     = longZipWith overl empty_sgl empty_bar xs ys 
    | n > 1      = longZipWith overl empty_sgl empty_bar xs (replicate n empty_bar ++ ys) 
    | otherwise  = error $ "zipOverlay - unreachable"
  where
    overl :: Overlay a -> Bar a -> Overlay a
    overl o          b   | nullBar b = o   -- no update
    overl (Single x) b   | nullBar x = Single b   -- swap
                         | otherwise = Multi [x,b]
    overl (Multi os) b               = Multi $ os++[b] -- yes really do an append!              
    
    nullBar (Bar [])      = True
    nullBar _             = False  
    empty_bar             = Bar []
    empty_sgl             = Single empty_bar        

                         