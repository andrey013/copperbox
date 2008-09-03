
--------------------------------------------------------------------------------
-- |
-- Module      :  HNotate.EventTree
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

module HNotate.EventTree (
    -- * Datatypes
    System(..), EventTree(..), EvtPosition(..),
    system, systemL, system1,
    
    root, event,
    
    par, prefix, poly, 
    
    repeated,
    
    onEventTree, onEventTreeM
    
  ) where


import qualified Data.Map as Map
import Data.Monoid
import Data.Sequence
import qualified Data.Foldable as F

-- Tracks in MIDI and multiple staffs are represented as a list of
-- Event trees
newtype System evt = System { getSystem :: Map.Map String (EventTree evt) }
  deriving Show

newtype EventTree evt = EventTree { getEventTree :: Seq (EvtPosition evt) }
  deriving Show


data EvtPosition evt = Evt evt
                     | StartPar | EndPar
                     | StartPre | EndPre
                     | Poly [EventTree evt]
  deriving Show

instance Monoid (System evt) where
  mempty = System mempty
  mappend a b = System $ (getSystem a) `Map.union` (getSystem b)
  
instance Monoid (EventTree evt) where
  mempty = EventTree mempty
  mappend a b = EventTree $ (getEventTree a) >< (getEventTree b)
  
instance Functor EventTree where
  fmap f (EventTree se)     = EventTree (fmap (fmap f) se)

instance Functor EvtPosition where
  fmap f (Evt e)            = Evt (f e)
  fmap f StartPar           = StartPar
  fmap f EndPar             = EndPar
  fmap f StartPre           = StartPre
  fmap f EndPre             = EndPre
  fmap f (Poly ts)          = Poly (fmap (fmap f) ts)


instance F.Foldable EventTree where
  foldMap f (EventTree se)  = F.foldMap (F.foldMap f) se

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


(|*>) :: EventTree evt -> EvtPosition evt -> EventTree evt
(|*>) (EventTree t) evt = EventTree $ t |> evt



system :: System evt
system = System mempty

systemL :: [(String,EventTree evt)] -> System evt
systemL = System . Map.fromList

system1 :: String -> EventTree evt -> System evt
system1 k t = System $ Map.insert k t mempty

root :: EventTree evt
root = EventTree empty

event           :: evt -> EventTree evt -> EventTree evt
event e t       = t |*> Evt e

par           :: [evt] -> EventTree evt -> EventTree evt
par [] t      = t
par es t      = seal $ foldl (flip event) (t |*> StartPar) es
  where
    seal t      = t |*> EndPar

prefix           :: [evt] -> EventTree evt -> EventTree evt
prefix [] t      = t
prefix es t      = seal $ foldl (flip event) (t |*> StartPre) es
  where
    seal t      = t |*> EndPre


-- poly does some optimizing ...
poly            :: [EventTree evt] -> EventTree evt -> EventTree evt
poly []  t      = t
poly [x] t      = F.foldl (flip event) t x
poly ts  t      = t |*> (Poly ts)

repeated ::
  Int -> (EventTree evt -> EventTree evt) -> (EventTree evt -> EventTree evt)
repeated i f = applyi i f
  where 
    applyi i f a | i < 1 = a
                 | otherwise  = applyi (i-1) f (f a)



onEventTree (System mp) k f = case Map.lookup k mp of
    Just v -> Just $ f v
    _ -> Nothing

onEventTreeM (System mp) k mf = case Map.lookup k mp of
    Just v -> mf v >>= return . Just
    _ -> return Nothing


