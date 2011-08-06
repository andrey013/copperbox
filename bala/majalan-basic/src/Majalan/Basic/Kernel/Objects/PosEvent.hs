{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Majalan.Basic.Kernel.Objects.PosEvent
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  highly unstable
-- Portability :  GHC 
--
-- Extended Event object.
--
-- Note - this object is probably misnamed.
--
-- In Wumpus, some objects are obviously drawn from different 
-- start positions (circles - center, direction-zero text - 
-- baseline left). However, there don\'t seem to be any valuable 
-- events that have \"start\" positions different to actual start.
-- 
--
--------------------------------------------------------------------------------

module Majalan.Basic.Kernel.Objects.PosEvent
  (

  -- * Positionable event
    PosEvent
  , DPosEvent


  -- * Operations
  , runPosEvent


  , makePosEvent
  , emptyPosEvent

  , extendPosEvent
 
  ) where


import Majalan.Basic.Kernel.Base.BaseDefs
import Majalan.Basic.Kernel.Base.Context
import Majalan.Basic.Kernel.Base.WrappedPrimitive
import Majalan.Basic.Kernel.Objects.Basis
import Majalan.Basic.Kernel.Objects.Concat
import Majalan.Basic.Kernel.Objects.LocEvent
import Majalan.Basic.Kernel.Objects.Orientation

import Majalan.Core                             -- package: majalan-core

import Control.Applicative
import Data.Monoid





type DOrtt = Orientation Double

-- Want to run a PosEvent and produce a LocEvent


-- | A positionable event.
--
newtype PosEvent ctx u a = PosEvent
          { getPosEvent :: Context ctx -> OnsetDbl -> (a, DOrtt, CatPrim ) }

type instance DUnit (PosEvent ctx u a) = u
type instance UCtx  (PosEvent ctx u)   = ctx
    
-- | Version of PosObject specialized to Double for the unit type.
--
type DPosEvent ctx = PosEvent ctx Double



instance Num u => Functor (PosEvent ctx u) where
  fmap f mf = PosEvent $ \r o -> 
              let (a,v1,w1) = getPosEvent mf r o in (f a,v1,w1)





instance InterpretUnit u => Applicative (PosEvent ctx u) where
  pure a    = PosEvent $ \_ _ -> (a,mempty,mempty)
  mf <*> ma = PosEvent $ \r o -> 
              let (f,v1,w1) = getPosEvent mf r o
                  (a,v2,w2) = getPosEvent ma r o
              in (f a, v1 `mappend` v2, w1 `mappend` w2)



instance InterpretUnit u => Monad (PosEvent ctx u) where
  return a  = PosEvent $ \_ _ -> (a,mempty,mempty)
  mf >>= k  = PosEvent $ \r o -> 
              let (a,v1,w1) = getPosEvent mf r o
                  (b,v2,w2) = getPosEvent (k a) r o
              in (b, v1 `mappend` v2, w1 `mappend` w2)


instance InterpretUnit u => ContextM (PosEvent ctx u) where
  askCtx          = PosEvent $ \r _ -> (r, mempty, mempty)
  asksCtx fn      = PosEvent $ \r _ -> (fn r, mempty, mempty)
  localize upd ma = PosEvent $ \r o -> getPosEvent ma (upd r) o


instance (Monoid a, InterpretUnit u) => Monoid (PosEvent ctx u a) where
  mempty = PosEvent $ \_ _ -> (mempty, mempty, mempty)
  ma `mappend` mb = PosEvent $ \r o -> 
                    let (a,v1,w1) = getPosEvent ma r o
                        (b,v2,w2) = getPosEvent mb r o
                    in (a `mappend` b, v1 `mappend` v2, w1 `mappend` w2)




runPosEvent :: InterpretUnit u => PosEvent ctx u a -> LocEvent ctx u a
runPosEvent mf = promoteLoc $ \ot -> 
    askCtx >>= \ctx -> 
    let dot      = normalize (ctx_tempo ctx) ot
        (a,_,ca) = getPosEvent mf ctx dot
    in replaceAns a $ primEvent ca
  




--------------------------------------------------------------------------------



-- | 'makePosEvent' : @ object_pos * loc_image -> PosEvent @ 
--
-- Create a 'PosEvent' from its length and 'LocEvent'.
--
makePosEvent :: InterpretUnit u
             => Query ctx u (Orientation u) -> ULocEvent ctx u 
             -> PosEvent ctx u (UNil u)
makePosEvent qortt gf = PosEvent $ \r o -> 
      let ortt   = runQuery r qortt
          dxmin  = normalize (ctx_tempo r) $ or_x_minor ortt
          dxmaj  = normalize (ctx_tempo r) $ or_x_major ortt
          uo     = dinterp (ctx_tempo r) o
          (a,w1) = runLocEvent uo r gf
      in (a, Orientation dxmin dxmaj, w1)



-- | 'emptyPosEvent' : @ PosEvent @
--
-- Build an empty 'PosGraphicObject'.
--
emptyPosEvent :: (Monoid a, InterpretUnit u)  => PosEvent ctx u a
emptyPosEvent = PosEvent $ \_ _ -> (mempty, mempty, mempty)



-- | Extend the orientation.
--
extendPosEvent :: InterpretUnit u 
               => u -> u -> PosEvent ctx u a -> PosEvent ctx u a
extendPosEvent t0 t1 po = PosEvent $ \r o -> 
    let (a,v1,w1) = getPosEvent po r o
        dt0       = normalize (ctx_tempo r) t0
        dt1       = normalize (ctx_tempo r) t1
        v2        = extendOrientation dt0 dt1 v1         
        w2        = moveCatPrim (negate dt0) w1
    in (a,v2,w2)




--------------------------------------------------------------------------------
-- Combining PosEvent


instance Monoid a => Overlay (PosEvent ctx u a) where
  overlay = centerOverlay


centerOverlay :: (Monoid a) 
              => PosEvent ctx u a -> PosEvent ctx u a -> PosEvent ctx u a
centerOverlay pa pb = PosEvent $ \r o -> 
    let (a,v1,w1) = getPosEvent pa r o
        (b,v2,w2) = getPosEvent pb r o
        (dv,ortt) = mergeCenters v1 v2
        w2'       = moveCatPrim dv w2
    in (a `mappend` b, ortt, w1 `mappend` w2')
        



instance Monoid a => Append (PosEvent ctx u a) where
  append = appendEndToEnd


appendEndToEnd :: (Monoid a)
               => PosEvent ctx u a -> PosEvent ctx u a -> PosEvent ctx u a
appendEndToEnd pa pb = PosEvent $ \ r o ->
    let (a,v1,w1) = getPosEvent pa r o
        (b,v2,w2) = getPosEvent pb r o
        (dv,ortt) = mergeHLine v1 v2
        w2'       = moveCatPrim dv w2
    in (a `mappend` b, ortt, w1 `mappend` w2')



instance (Monoid a, InterpretUnit u) => Space (PosEvent ctx u a) where
  space = spaceEndToEnd


spaceEndToEnd :: (Monoid a, InterpretUnit u)
              => u -> PosEvent ctx u a -> PosEvent ctx u a -> PosEvent ctx u a
spaceEndToEnd u a b = a `append` blank `append` b
  where
    blank = replaceAns mempty $ makePosEvent (pure $ Orientation 0 u) mempty

