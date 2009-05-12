{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Mullein.Bracket
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Bar splitting and beam grouping.
--
--------------------------------------------------------------------------------



module Mullein.Bracket ( 
  bracket,
  
  ) where

import Mullein.Core
import Mullein.Duration
import Mullein.Utils

import qualified Data.DList as DL
import Data.List (foldl')
import Data.Monoid
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

{-
-- TODO bracketing with anacrusis  
bracket1Ana :: Duration -> MetricalSpec -> [ElementP e] -> [BarP e]
bracket1Ana anacrusis mspec notes = partitionAndBeam bs bss notes where
    (bs,bss) = repeatSpec anacrusis mspec     
-}

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

-- Note for LilyPond - graces cannot start or end a beam group.

beam :: [Duration] -> [ElementP e] -> [BracketP e]
beam = DL.toList `oo` unfoldrMonoid2 fn where
    -- notes exhausted, finish the unfold
    fn _           []         = Nothing

    -- beam lengths exhausted, package up the remaining items as singletons
    fn []          xs         = let trail = DL.fromList $ map Singleton xs
                                in Just (trail,[],[]) 
                                
    fn dstk       (x:xs)  
        | duration x >= (1%4) || not (noc x) 
                              = Just (sglD x, reduceStk (duration x) dstk, xs)

    fn dstk@(d:_) xs          = let (count,l,r) = beamGroup1 d xs in 
                                Just (l, reduceStk count dstk, r)
                                

beamGroup1 :: Duration 
              -> [ElementP e] 
              -> (Duration, DL.DList (BracketP e), [ElementP e])
beamGroup1 d0 = step d0 DL.empty [] where
    
    -- input exahusted 
    step d dacc stk []        = (d, dacc `mappend` unwindStack stk, []) 

    -- duration 0 - end of beam group    
    step 0 dacc stk xs        = (0, dacc `mappend` unwindStack stk, xs)
 
    -- element too big for a beam group, make it a singleton
    step d dacc stk (e:es) 
         | duration e > d     = let dlist = dacc `mappend` unwindStack stk
                                                 `mappend` sglD e 
                                in (d - duration e, dlist, es)

    -- element too big to be beamed, but still 'fits' in the beam group
    step d dacc stk (e:es) 
         | duration e >= 1%4  = let dacc' = dacc `mappend` unwindStack stk
                                                 `mappend` sglD e 
                                in step (d - duration e) dacc' [] es

    
    -- cannot start the stack w                      
    step d dacc []  (e:es) 
         | noc e              = step (d - duration e) dacc [e] es
         | otherwise          = step (d - duration e) (dacc `mappend` sglD e) [] es

    -- at this point - element dur must be smaller than d and stk has elements
    -- so cons it to the stack (as the stk should be in reverse order)
    step d dacc stk (e:es)    = step (d - duration e) dacc (e:stk) es 


-- stack is reversed so we can see whether or not the end has 
-- notes or chords 
unwindStack :: [ElementP e] -> DL.DList (BracketP e)
unwindStack []                     = DL.empty
unwindStack (e:es) | noc e         = DL.singleton $ mkBracket $ reverse (e:es)
                   | otherwise     = unwindStack es `mappend` sglD e

-- noc - note or chord
noc :: ElementP e -> Bool
noc (Note _ _)  = True
noc (Chord _ _) = True
noc _           = False

sglD :: ElementP e -> DL.DList (BracketP e)
sglD = DL.singleton . Singleton

mkBracket :: [ElementP e] -> BracketP e
mkBracket [x] = Singleton x
mkBracket xs  = Bracket xs 


-- cannot reduce stack by negative amounts...
reduceStk :: Duration -> [Duration] -> [Duration]
reduceStk d stk     | d <= 0    = stk       
reduceStk _ []                  = []
reduceStk d (x:xs)  | d == x    = xs
                    | d > x     = reduceStk (d-x) xs
                    | otherwise = (d-x):xs 
                      
  
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

