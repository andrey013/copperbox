
--------------------------------------------------------------------------------
-- |
-- Module      :  HNotate.NoteList
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

module HNotate.EventList (
    -- * Datatypes
    System, EventList(..), EvtPosition(..),
    system, systemL, system1,
    
    root, event,
    
    par, prefix, poly, 
    
    repeated,
    
    eventlist,

    
  ) where


import qualified Data.Map as Map
import Data.Monoid
import Data.Sequence
import qualified Data.Foldable as F


type System evt = Map.Map String (EventList evt)


newtype EventList evt = EventList { getEventList :: Seq (EvtPosition evt) }
  deriving Show


data EvtPosition evt = Evt evt
                     | StartPar | EndPar
                     | StartPre | EndPre
                     | Poly [EventList evt]
  deriving Show


  
instance Monoid (EventList evt) where
  mempty = EventList mempty
  mappend a b = EventList $ (getEventList a) >< (getEventList b)
  
instance Functor EventList where
  fmap f (EventList se)     = EventList (fmap (fmap f) se)

instance Functor EvtPosition where
  fmap f (Evt e)            = Evt (f e)
  fmap f StartPar           = StartPar
  fmap f EndPar             = EndPar
  fmap f StartPre           = StartPre
  fmap f EndPre             = EndPre
  fmap f (Poly ts)          = Poly (fmap (fmap f) ts)


instance F.Foldable EventList where
  foldMap f (EventList se)  = F.foldMap (F.foldMap f) se

instance F.Foldable EvtPosition where
  foldMap f (Evt e)         = f e
  foldMap f StartPar        = mempty
  foldMap f EndPar          = mempty
  foldMap f StartPre        = mempty
  foldMap f EndPre          = mempty
  foldMap f (Poly ts)       = F.foldMap (F.foldMap f) ts



-- \# or .\#
-- c.f. show and shows

infixl 7 #

x # f = f x


infixl 7 #.

g #. f = f . g


(|*>) :: EventList evt -> EvtPosition evt -> EventList evt
(|*>) (EventList t) evt = EventList $ t |> evt



system :: System evt
system = mempty

systemL :: [(String, EventList evt)] -> System evt
systemL = Map.fromList

system1 :: String -> EventList evt -> System evt
system1 k t = Map.insert k t mempty

root :: EventList evt
root = EventList empty

event           :: evt -> EventList evt -> EventList evt
event e t       = t |*> Evt e

par           :: [evt] -> EventList evt -> EventList evt
par [] t      = t
par es t      = seal $ foldl (flip event) (t |*> StartPar) es
  where
    seal t      = t |*> EndPar

prefix           :: [evt] -> EventList evt -> EventList evt
prefix [] t      = t
prefix es t      = seal $ foldl (flip event) (t |*> StartPre) es
  where
    seal t      = t |*> EndPre


-- poly does some optimizing ...
poly            :: [EventList evt] -> EventList evt -> EventList evt
poly []  t      = t
poly [x] t      = F.foldl (flip event) t x
poly ts  t      = t |*> (Poly ts)

repeated :: Int -> (EventList evt -> EventList evt) -> (EventList evt -> EventList evt)
repeated i f = applyi i f
  where 
    applyi i f a | i < 1 = a
                 | otherwise  = applyi (i-1) f (f a)

eventlist :: [evt] -> EventList evt
eventlist = foldl (flip event) root 


