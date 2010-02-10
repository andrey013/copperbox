{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Neume.StateMap
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- A stateful variant of Functor/fmap
--
-- Experiment to see if it has any convenience over fmapM & a 
-- state monad. 
--
--------------------------------------------------------------------------------


module Neume.StateMap 
  ( 
  -- * Stateful map
    StateMap(..)
  , StateMap2(..)
  , StateMap3(..)

  ) where 

import Neume.OneList


class StateMap f where
  stmap :: (a -> st -> (b,st)) -> f a -> st -> (f b,st)


-- Bifunctor...
class StateMap2 f where
  stmap2 :: (a -> st -> (u,st)) -> (b -> st -> (v,st)) -> f a b -> st -> (f u v, st)
 
class StateMap3 f where
  stmap3 :: (a -> st -> (u,st)) -> (b -> st -> (v,st)) -> (c -> st -> (w,st)) 
         -> f a b c -> st -> (f u v w, st)



instance StateMap [] where
  stmap _ []     st = ([],st)
  stmap f (x:xs) st = (b:bs,st'') where (b,st')   = f x st  
                                        (bs,st'') = stmap f xs st'

instance StateMap Maybe where
  stmap _ Nothing  st = (Nothing,st)
  stmap f (Just a) st = (Just a',st') where (a',st') = f a st

instance StateMap ((,) a) where
  stmap f (x,y) st   = ((x,y'),st') where (y',st') = f y st

instance StateMap OneList where
  stmap f os s0 = step (viewl os) s0 where
    step (OneL a)   st = (one a', st')     where (a',st')  = f a st
    step (a :<< as) st = (cons b bs,st'')  where (b,st')   = f a st  
                                                 (bs,st'') = step (viewl as) st'


