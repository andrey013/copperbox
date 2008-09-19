{-# LANGUAGE FlexibleInstances, TypeSynonymInstances,
             MultiParamTypeClasses #-}
             
--------------------------------------------------------------------------------
-- |
-- Module      :  HNotateHaskore.HaskoreInterface
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  Flexible instances, mptc.
--
-- An instance of the Event class for Haskore's pitch and duration 
-- representation.
--
--------------------------------------------------------------------------------



module HNotateHaskore.HaskoreInterface where

import HNotate.Duration
import HNotate.NoteList
import HNotate.Pitch

import qualified Haskore as H 
import Haskore ( Music(..), Player(..), defPlayer ) 

import Data.List(partition, groupBy, sort)
import Data.Monoid
import Data.Ratio
import Data.Sequence 


infixl 7 #

x # f = f x


  
  

-- Rests have been lost in Haskore performance
-- (Trills will be very difficult to recover...)


psystem :: H.Performance -> System
psystem p = let split_list = splitByInst p
            in systemL $ map (\(n,p) -> (show n, instEL (n,p))) split_list

instrumentNames :: H.Performance -> [String]
instrumentNames = map (show . fst) . splitByInst
  
instEL :: (H.InstrumentName,H.Performance) -> EventList
instEL (n,p) = snd $ foldl fn (0%1,root) $ groupChords p
  where
    fn (onset,tree) e   = let (e_onset, e_dur, f) = evt e in 
        if e_onset == onset
          then (e_onset + e_dur, tree # f)
          else let r = convert (e_onset - onset)
               in (e_onset + e_dur, tree # rest r # f)

groupChords :: [H.Event] -> [[H.Event]]
groupChords = groupBy (\a b -> H.eTime a == H.eTime b)

    
evt :: [H.Event] -> (H.Time, H.DurT, EventList -> EventList)
evt [e]     = event1 e
evt (e:es)  = let chord_notes       = sort $ map convert (e:es)
                  e_dur             = H.eDur e
                  e_onset           = H.eTime e
              in (e_onset, e_dur, chord chord_notes (convert e_dur))
          
event1 :: H.Event -> (H.Time, H.DurT, EventList -> EventList)
event1 e@(H.Event {H.eTime=onset, H.eDur=drn}) = 
    (onset,drn, note (convert e) (convert drn))


-- From Haskore, ToMidi.lhs -- not an exposed function
splitByInst :: H.Performance ->  [(H.InstrumentName,H.Performance)]
splitByInst [] = []
splitByInst pf = (i,pf1) : splitByInst pf2
  where i         = H.eInst (head pf)
        (pf1,pf2) = partition (\e -> H.eInst e == i) pf
    


class Convert a b where
  convert :: a -> b 


instance Convert H.Event Pitch where
  convert (H.Event {H.ePitch = p}) = convert p

instance Convert H.AbsPitch Pitch where
  convert = fromSemitones
  

instance Convert H.Dur Duration where
  convert r = let (n,d) = (numerator r, denominator r)
              in printableDurationF (fromIntegral n % fromIntegral d)

instance Convert (H.PitchClass,Int) Pitch where
  convert (H.Cf, o)    = Pitch C Flat o
  convert (H.C,  o)    = Pitch C Nat o
  convert (H.Cs, o)    = Pitch C Sharp o
  convert (H.Df, o)    = Pitch D Flat o
  convert (H.D,  o)    = Pitch D Nat o
  convert (H.Ds, o)    = Pitch D Sharp o
  convert (H.Ef, o)    = Pitch E Flat o
  convert (H.E,  o)    = Pitch E Nat o
  convert (H.Es, o)    = Pitch E Sharp o
  convert (H.Ff, o)    = Pitch F Flat o
  convert (H.F,  o)    = Pitch F Nat o
  convert (H.Fs, o)    = Pitch F Sharp o
  convert (H.Gf, o)    = Pitch G Flat o
  convert (H.G,  o)    = Pitch G Nat o
  convert (H.Gs, o)    = Pitch G Sharp o
  convert (H.Af, o)    = Pitch A Flat o
  convert (H.A,  o)    = Pitch A Nat o
  convert (H.As, o)    = Pitch A Sharp o
  convert (H.Bf, o)    = Pitch B Flat o
  convert (H.B,  o)    = Pitch B Nat o
  convert (H.Bs, o)    = Pitch B Sharp o






