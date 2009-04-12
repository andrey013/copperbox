{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Mullein.Section
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



module Mullein.Section where

import Mullein.Core ( meterFraction )
import Mullein.CoreTypes
import Mullein.Duration
import Mullein.Gen.Syntax
import Mullein.Utils

import Data.Ratio
import Data.Sequence ( Seq, ViewL (..), ( <| ), viewl, empty )
import qualified Data.Sequence as S

data TieStatus = Tied | NotTied
  deriving (Eq,Show)

section :: MetricalSpec -> Seq Element -> Section
section mspec notes = Section $ partitionAndBeam bs bss notes where
    (bs,bss) = repeatSpec 0 mspec   

  
sectionAna :: Duration -> MetricalSpec -> Seq Element -> Section
sectionAna anacrusis mspec notes = 
    Section $ partitionAndBeam bs bss notes
  where
    (bs,bss) = repeatSpec anacrusis mspec     


repeatSpec :: Duration -> MetricalSpec -> ([Duration],[[Duration]])
repeatSpec 0 (b,bs) = (repeat $ meterFraction b, repeat bs)
repeatSpec a (b,bs) = (reduceStk a ds, reduceStk a bs : repeat bs) where 
    ds = repeat $ meterFraction b

             
partitionAndBeam :: [Duration] -> [[Duration]] -> Seq Element -> [Bar]
partitionAndBeam ds_bar dss_beam notelist = 
    zipWith fn (divideToBars ds_bar notelist) dss_beam 
  where    
    fn (_,[])        _  = Bar $ VoiceUnit [] False
    fn (Tied, notes) xs = Bar $ VoiceUnit (beam xs notes) True 
    fn (_, notes)    xs = Bar $ VoiceUnit (beam xs notes) False

    
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
 
beam :: [Duration] -> [Element] -> [Pulsation]
beam = unfoldr2 fn where
    -- notes exhausted
    fn _          []          = Nothing

    -- beam lengths exhausted, return singletons 
    fn []         (x:xs)      = Just (SingleElt x, [],xs)

    fn dstk       (x:xs)  
        | duration x > (1%8)  = Just (SingleElt x, reduceStk (duration x) dstk, xs)
    
    fn dstk@(d:_) xs          = let (count,l,r) = beamGroup1 d xs in 
                                Just (BeamedGroup l, reduceStk count dstk, r)


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

zipOverlays :: Section -> Section -> Section
zipOverlays (Section bs) (Section bs') = Section $ zipWith f bs bs' where
    f (Bar v)        b2    = if null vs then Bar v else Overlay v vs where
                                 vs = voices b2
    f (Overlay v vs) b2    = Overlay v (vs ++ voices b2) 

    voices (Bar v)         = if nullVoice v then [] else [v]
    voices (Overlay v vs)  = v:vs
    
nullVoice :: VoiceUnit -> Bool
nullVoice (VoiceUnit xs _) = null xs

