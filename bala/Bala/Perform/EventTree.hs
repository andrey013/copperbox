
--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Perform.EventTree
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- A tree representation in a sequence. 
--
--------------------------------------------------------------------------------

module Bala.Perform.EventTree where

import Bala.Base.BaseExtra (applyi)

import Data.Sequence

-- Tracks in MIDI and multiple staffs are represented as a list of
-- Event trees
newtype Performance evt = Perf { unPerf :: [EventTree evt] }
  deriving Show
  
type EventTree evt = Seq (EvtPosition evt)


data EvtPosition evt = Evt evt 
                     | StartPar | EndPar
                     | StartPre | EndPre
                     | Sequence [EventTree evt]
  deriving Show

instance Functor EvtPosition where
  fmap f (Evt e)        = Evt (f e) 
  fmap f StartPar       = StartPar
  fmap f EndPar         = EndPar
  fmap f StartPre       = StartPre
  fmap f EndPre         = EndPre  
  fmap f (Sequence ts)  = Sequence (map (fmap (fmap f)) ts) 

  

  
-- \# or .\# 
-- c.f. show and shows   

infixl 7 #

x # f = f x


infixl 7 #.

g #. f = f . g




root :: EventTree evt
root = empty

event           :: evt -> EventTree evt -> EventTree evt
event e t       = t |> Evt e

chord           :: [evt] -> EventTree evt -> EventTree evt
chord [] t      = t
chord es t      = seal $ foldl (flip event) (t |> StartPar) es
  where 
    seal t      = t |> EndPar 

grace           :: [evt] -> EventTree evt -> EventTree evt
grace [] t      = t
grace es t      = seal $ foldl (flip event) (t |> StartPre) es
  where 
    seal t      = t |> EndPre

parallel        :: [EventTree evt] -> EventTree evt -> EventTree evt
parallel [] t   = t
parallel ts t   = t |> (Sequence ts)

repeated :: 
  Int -> (EventTree evt -> EventTree evt) -> (EventTree evt -> EventTree evt)
repeated i f = applyi i f 


( >#< )         :: EventTree evt -> EventTree evt -> EventTree evt
a >#< b         = a >< b
               