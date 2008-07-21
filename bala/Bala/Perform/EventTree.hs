
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

import Bala.Base.BaseExtra (applyi, Affi(..), listS )

import Data.Monoid
import Data.Sequence
import qualified Data.Foldable as F

-- Tracks in MIDI and multiple staffs are represented as a list of
-- Event trees
newtype Performance evt = Perf { unPerf :: [EventTree evt] }
  deriving Show
  
newtype EventTree evt = ET { unET :: Seq (EvtPosition evt) }
  deriving Show


data EvtPosition evt = Evt evt 
                     | StartPar | EndPar
                     | StartPre | EndPre
                     | Sequence [EventTree evt]
  deriving Show
  
instance Functor EventTree where
  fmap f (ET sq)        = ET (fmap (fmap f) sq)

instance Functor EvtPosition where
  fmap f (Evt e)        = Evt (f e) 
  fmap f StartPar       = StartPar
  fmap f EndPar         = EndPar
  fmap f StartPre       = StartPre
  fmap f EndPre         = EndPre  
  fmap f (Sequence ts)  = Sequence (fmap (fmap f) ts) 
  
    
instance F.Foldable EventTree where
  foldMap f (ET sq)       = F.foldMap (F.foldMap f) sq

instance F.Foldable EvtPosition where
  foldMap f (Evt e)        = f e 
  foldMap f StartPar       = mempty
  foldMap f EndPar         = mempty
  foldMap f StartPre       = mempty
  foldMap f EndPre         = mempty  
  foldMap f (Sequence ts)  = F.foldMap (F.foldMap f) ts
  
  




  

  
-- \# or .\# 
-- c.f. show and shows   

infixl 7 #

x # f = f x


infixl 7 #.

g #. f = f . g

(|*>) (ET t) evt = ET $ t |> evt


perf :: [EventTree evt] -> Performance evt
perf = Perf

root :: EventTree evt
root = ET empty

event           :: evt -> EventTree evt -> EventTree evt
event e t       = t |*> Evt e

chord           :: [evt] -> EventTree evt -> EventTree evt
chord [] t      = t
chord es t      = seal $ foldl (flip event) (t |*> StartPar) es
  where 
    seal t      = t |*> EndPar 

grace           :: [evt] -> EventTree evt -> EventTree evt
grace [] t      = t
grace es t      = seal $ foldl (flip event) (t |*> StartPre) es
  where 
    seal t      = t |*> EndPre

parallel        :: [EventTree evt] -> EventTree evt -> EventTree evt
parallel [] t   = t
parallel ts t   = t |*> (Sequence ts)

repeated :: 
  Int -> (EventTree evt -> EventTree evt) -> (EventTree evt -> EventTree evt)
repeated i f = applyi i f 


( >#< )         :: EventTree evt -> EventTree evt -> EventTree evt
a >#< b         = ET $ (unET a) >< (unET b)


instance (Affi evt) => Affi (EvtPosition evt) where
  affi (Evt evt)      = affi evt
  affi StartPar       = showString "'<'"
  affi EndPar         = showString "'>'"
  affi StartPre       = showString "'{'"
  affi EndPre         = showString "'}'"
  affi (Sequence es)  = listS (map affi es)    -- needs improving
  
instance (Affi evt) => Affi (EventTree evt) where
  affi (ET sq)        = listS $ F.foldr fn [] sq
    where fn e a = affi e : a                   