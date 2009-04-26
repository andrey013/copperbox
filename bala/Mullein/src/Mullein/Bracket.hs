{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Mullein.Bracket
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined
--
-- Bar splitting and beam grouping.
--
--------------------------------------------------------------------------------



module Mullein.Bracket where

import Mullein.Core
import Mullein.CoreTypes
import Mullein.Duration
import Mullein.Utils

import Data.List (foldl')
import Data.Ratio

data TieStatus = Tied | NotTied
  deriving (Eq,Show)


bracket :: Key -> MetricalSpec -> OverlayList e -> MotifP e
bracket k mspec (p,xs) = Motif k time_sig $ foldl' zipOverlays prime ovs
  where
    prime    = bracket1 mspec p
    ovs      = map (prod id (bracket1 mspec)) xs
    time_sig = fst mspec


bracket1 :: MetricalSpec -> [ElementP e] -> [BarP e]
bracket1 mspec notes = partitionAndBeam bs bss notes where
    (bs,bss) = repeatSpec 0 mspec   

-- TODO bracketing with anacrusis  
bracket1Ana :: Duration -> MetricalSpec -> [ElementP e] -> [BarP e]
bracket1Ana anacrusis mspec notes = partitionAndBeam bs bss notes where
    (bs,bss) = repeatSpec anacrusis mspec     


repeatSpec :: Duration -> MetricalSpec -> ([Duration],[[Duration]])
repeatSpec 0 (b,bs) = (repeat $ meterFraction b, repeat bs)
repeatSpec a (b,bs) = (reduceStk a ds, reduceStk a bs : repeat bs) where 
    ds = repeat $ meterFraction b

             
partitionAndBeam :: [Duration] -> [[Duration]] -> [ElementP e] -> [BarP e]
partitionAndBeam ds_bar dss_beam notes = 
    zipWith fn (divideToBars ds_bar notes) dss_beam 
  where    
    fn (_,[])      _  = Bar $ Unison [] False
    fn (Tied, bar) ds = Bar $ Unison (beam ds bar) True 
    fn (_, bar)    ds = Bar $ Unison (beam ds bar) False

    
-- split a list of notes into bars of given duration 
divideToBars :: Temporal a => [Duration] -> [a] -> [(TieStatus, [a])] 
divideToBars ds ns = fst $ anaMap fn (NotTied,ns) ds where
  fn _ (_,  []) = Nothing
  fn d (tie,xs) = let (ls, opt_split, rest) = fitTill d xs in
                  maybe (Just ((tie,ls), (NotTied, rest)))
                        (\split_note -> Just ((tie,ls), (Tied, split_note : rest)))
                        opt_split

  
fitTill :: Temporal a => Duration -> [a] -> ([a], Maybe a, [a])
fitTill d0 es = step d0 es where
    step _ []     = ([],Nothing,[]) 
    step d (x:xs) = let d' = duration x in 
                    case d' `compare` d of
                        EQ -> ([x],Nothing,xs)
                        LT -> (x:ls,pivot,rs) where
                                (ls,pivot,rs) = fitTill (d-d') xs
                        GT -> ([swapDuration d x], Just $ swapDuration (d'-d) x, xs)
                         
--------------------------------------------------------------------------------
-- beaming

-- beam splitting as an unfold. 
-- For syntactical clarity we use a 'double unfoldr' rather than 
-- tuple the two elements of state together.
-- The state is (1) the stack of durations for each beam group, 
-- and (2) the input stream of notes.
 
beam :: [Duration] -> [ElementP e] -> [BracketP e]
beam = unfoldr2 fn where
    -- notes exhausted
    fn _          []          = Nothing

    -- beam lengths exhausted, return singletons 
    fn []         (x:xs)      = Just (Singleton x, [],xs)

    fn dstk       (x:xs)  
        | duration x > (1%8)  = Just (Singleton x, reduceStk (duration x) dstk, xs)
    
    fn dstk@(d:_) xs          = let (count,l,r) = beamGroup1 d xs in 
                                Just (Bracket l, reduceStk count dstk, r)


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


---------------------------------------------------------------------------------
-- overlay

zipOverlays :: [BarP e] -> (BarNum,[BarP e]) -> [BarP e]
zipOverlays bs (bnum,bs') = prefix ++ longZipWith f id id suffix bs' where
    (prefix,suffix)        = splitAt bnum bs
    f (Bar v)        b2    = if null vs then Bar v else Overlay v vs where
                                 vs = voices b2
    f (Overlay v vs) b2    = Overlay v (vs ++ voices b2) 

    voices (Bar v)         = if nullVoice v then [] else [v]
    voices (Overlay v vs)  = v:vs
    
nullVoice :: UnisonP e -> Bool
nullVoice (Unison xs _) = null xs

