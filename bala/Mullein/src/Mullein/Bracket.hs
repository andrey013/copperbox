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



module Mullein.Bracket where

import Mullein.Core
import Mullein.Duration
import Mullein.Utils


import Data.OneMany

import Data.List ( foldl' )
import Data.Ratio


data Hyphenated = Hyphenated | NotHyphenated
  deriving (Eq,Show)


data Step a s = Done | Skip s | Yield a s
  deriving (Eq)

data Binary a = One a | Two a a
  deriving (Eq,Show)


apoSkipList :: (st -> a -> Step b st) -> (st -> [a] -> [b]) -> st -> [a] -> [b]
apoSkipList f g st0 xs0 = step st0 xs0 where
  step s []     = g s []
  step s (x:xs) = case f s x of
                    Done       -> g s (x:xs)     -- x must be re-queued
                    Skip s'    -> step s' xs
                    Yield a s' -> a : step s' xs
           

-- | Skipping apomorphism that is unfolded /along/ a list
-- Unfortunately, a single beam step can produce two results 
-- a. the current accumulation & b. a single note too long for a beam group
-- so we have to pollute what could be a general combinator @apoSkipList@ 
-- with the result wrapped in the Binary type.

apoSkipListB :: (st -> a -> Step (Binary b) st) -> (st -> [a] -> [b]) -> st -> [a] -> [b]
apoSkipListB f g st0 xs0 = step st0 xs0 where
  step s []     = g s []
  step s (x:xs) = case f s x of
                    Done               -> g s (x:xs)     -- x must be re-queued
                    Skip s'            -> step s' xs
                    Yield (One a) s'   -> a : step s' xs
                    Yield (Two a b) s' -> a : b :step s' xs


--------------------------------------------------------------------------------
-- bar & beam
-- type EP e = ElementP e

bracket' :: Temporal e => MeterPattern -> [e] -> [(Hyphenated,[OneMany e])]
bracket' mp notes = map beamer $ bar (sum mp) notes 
  where 
    beamer (h,xs) = (h, beam mp xs)


bracket :: Key -> MetricalSpec -> OverlayList e -> MotifP e
bracket k mspec (p,xs) = Motif k time_sig $ foldl' zipOverlays prime ovs
  where
    prime    = [] -- convToOld $ bracket' (snd mspec) p
    ovs      = [] -- map (prod id (convToOld . bracket' (snd mspec))) xs
    time_sig = fst mspec

convToOld :: [(Hyphenated,[OneMany e])] -> [BarP e]
convToOld = undefined

--------------------------------------------------------------------------------
-- bar

bar :: Temporal e => Duration -> [e] -> [(Hyphenated,[e])]
bar bar_len ns = apoSkipList (barStep bar_len) barFlush ([],0) ns


barStep :: Temporal e
        => Duration -> ([e],Duration) -> e
        -> Step (Hyphenated,[e]) ([e],Duration)
barStep bar_len (ca,i) note = step (duration note) where
  step n | i+n > bar_len  = let (l,r) = split i note in 
                            Yield (Hyphenated, reverse $ l:ca) ([r],duration r)
         | i+n == bar_len = Yield (NotHyphenated, reverse $ note:ca) ([],0)
         | otherwise      = Skip (note:ca,i+n)
 
split :: Temporal e => Duration -> e -> (e,e)
split i note = (swapDuration i note, swapDuration (n-i) note) 
  where n = duration note
  

-- 
barFlush :: ([e],Duration) -> [e] -> [(Hyphenated,[e])]
barFlush ([],_) _ = []
barFlush (ca,_) _ = [(NotHyphenated, reverse ca)]



--------------------------------------------------------------------------------
-- beam


beam :: Temporal e => MeterPattern -> [e] ->  [OneMany e]
beam mp notes = apoSkipListB beamStep beamFlush ([],mp) notes


beamFlush :: Temporal e => ([e],MeterPattern) -> [e] -> [OneMany e]
beamFlush ([],_)  rs = map one rs
beamFlush (cca,_) rs = fromList (reverse cca) : map one rs

beamStep :: Temporal e => ([e],MeterPattern) 
         -> e
         -> Step (Binary (OneMany e)) ([e],MeterPattern)
beamStep (ca,stk) e = maybe Done (sk (duration e)) (top stk) where
  sk n d | n >= 1%4 || n > d  = next ca     (Just e) (consume n stk)
         | n == d             = next (e:ca) Nothing  (consume n stk)
         | otherwise          = skip (e:ca) (consume n stk)

  next []  Nothing  ss = Skip ([],ss)
  next []  (Just a) ss = Yield (One (one a)) ([],ss)
  next cca (Just a) ss = Yield (Two (fromList $ reverse cca) (one a)) ([],ss)
  next cca _        ss = Yield (One (fromList $ reverse cca)) ([],ss)

  skip cca ss          = Skip (cca,ss) 




-- Maybe the top of the stack.
top :: [a] -> Maybe a
top (x:_) = Just x
top []    = Nothing


-- reduce the MeterPattern stack by the duration
consume :: (Num a, Ord a) => a -> [a] -> [a]
consume _ []                  = []
consume n (x:xs) | n < x      = (x-n):xs
                 | n == x     = xs
                 | otherwise  = consume (n-x) xs

                      

  
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

