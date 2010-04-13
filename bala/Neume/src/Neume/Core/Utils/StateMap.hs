{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Neume.Core.Utils.StateMap
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


module Neume.Core.Utils.StateMap 
  ( 

    stBinary
  , stTernary
  , caboose_stmap
  , anacrusis_stmap

  -- * Stateful map
  , StateMap(..)
  , StateMap2(..)
  , StateMap3(..)

  , stmap2a
  , stmap2b

  , stmap3a 
  , stmap3b
  , stmap3c

  ) where 

import Neume.Core.Utils.FunctorN
import Neume.Core.Utils.OneList

wrap :: a -> [a]
wrap = return



stBinary :: (x -> y -> ans) 
         -> (st -> a -> (x,st)) 
         -> (st -> b -> (y,st)) 
         -> st -> a -> b 
         -> (ans,st)
stBinary op fa fb st a b = (x `op` y, st'') 
  where
    (x,st')  = fa st  a
    (y,st'') = fb st' b


stTernary :: (x -> y -> z -> ans) 
         -> (st -> a -> (x,st)) 
         -> (st -> b -> (y,st)) 
         -> (st -> c -> (z,st)) 
         -> st -> a -> b -> c
         -> (ans,st)
stTernary op3 fa fb fc st a b c = (op3 x y z, st''') 
  where
    (x,st')   = fa st   a
    (y,st'')  = fb st'  b
    (z,st''') = fc st'' c 


-- just on lists...
--
-- The implemenation is a bit tricky as it uses a worker-wrapper
-- style list split to avoid a pattern match
--
caboose_stmap :: (st -> a -> (b,st)) -> (st -> a -> (b,st)) -> st -> [a] -> ([b],st)
caboose_stmap _ _ s0 []     = ([],s0)
caboose_stmap f g s0 (a:as) = step s0 a as where
  step st x []           = fmap2a wrap $ g st x
  step st x (y:ys)       = stBinary (:) f (\s' -> step s' y) st x ys


anacrusis_stmap :: (st -> a -> (b,st)) -> (st -> a -> (b,st)) -> st -> [a] -> ([b],st)
anacrusis_stmap _ _ st []     = ([],st)
anacrusis_stmap f g st (a:as) = stBinary (:) f (stmap g) st a as



--------------------------------------------------------------------------------

class StateMap f where
  stmap :: (st -> a -> (b,st)) -> st -> f a -> (f b,st)


-- Bifunctor...
class StateMap2 f where
  stmap2 :: (st -> a -> (u,st)) -> (st -> b -> (v,st)) -> st -> f a b -> (f u v, st)
 
class StateMap3 f where
  stmap3 :: (st -> a -> (u,st)) -> (st -> b -> (v,st)) -> (st -> c -> (w,st)) 
         -> st -> f a b c -> (f u v w, st)


stmap2a :: StateMap2 f => (st -> a -> (u,st)) -> st -> f a b -> (f u b, st)
stmap2a f = stmap2 f revpair

stmap2b :: StateMap2 f => (st -> b -> (v,st)) -> st -> f a b -> (f a v, st)
stmap2b g = stmap2 revpair g


stmap3a :: StateMap3 f => (st -> a -> (u,st)) -> st -> f a b c -> (f u b c, st)
stmap3a f1 = stmap3 f1 revpair revpair

stmap3b :: StateMap3 f => (st -> b -> (v,st)) -> st -> f a b c -> (f a v c, st)
stmap3b f2 = stmap3 revpair f2 revpair

stmap3c :: StateMap3 f => (st -> c -> (w,st)) -> st -> f a b c -> (f a b w, st)
stmap3c f3 = stmap3 revpair revpair f3

revpair :: a -> b -> (b,a) 
revpair = flip (,)

--------------------------------------------------------------------------------
-- Instances

instance StateMap [] where
  stmap _ st []     = ([],st)
  stmap f st (x:xs) = stBinary (:) f (stmap f) st x xs


instance StateMap Maybe where
  stmap _ st Nothing  = (Nothing,st)
  stmap f st (Just a) = fmap2a Just $ f st a


instance StateMap OneList where
  stmap f s0 os = step s0 (viewl os) where
    step st (OneL a)   = fmap2a one $ f st a
    step st (a :<< as) = stBinary cons f step st a (viewl as)


-- StateMap2

instance StateMap2 (,) where
  stmap2 f g st (x,y) = stBinary (,) f g st x y


instance StateMap2 Either where
  stmap2 f _ st (Left x)    = fmap2a Left  $ f st x
  stmap2 _ g st (Right y)   = fmap2a Right $ g st y



