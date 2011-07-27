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
-- Note - this object does not be seem to have a valuable 
-- use-case, and if one isn\'t found it will be removed.
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

    Bounds(..)

  -- * Positionable event

  , PosEvent
  , DPosEvent


  -- * Operations
  , runPosEvent

  , makePosEvent
  , emptyPosEvent


  , localPosEvent

  , elaboratePosEvent

  , extendPosEvent
 
  ) where


import Majalan.Basic.Kernel.Base.BaseDefs
import Majalan.Basic.Kernel.Base.Context
import Majalan.Basic.Kernel.Base.WrappedPrimitive
import Majalan.Basic.Kernel.Objects.Basis
import Majalan.Basic.Kernel.Objects.Concat
import Majalan.Basic.Kernel.Objects.LocEvent


import Control.Applicative
import Data.Monoid


-- Note - Orientation and BoundingBox are quite different 
-- objects in Wumpus. A BoundingBox is obviously not coordinate
-- free (and hence is not monoidal) whereas an Orientation is
-- coordinate free and monoidal.

data Bounds u = Bounds 
      { bounds_start :: u
      , bounds_end   :: u
      }
  deriving (Eq,Ord,Show)


type instance DUnit (Bounds u) = u

instance Functor Bounds where
  fmap f (Bounds s e) = Bounds (f s) (f e)




-- | Helper for PosEvent - a LocEvent that is /pre-applied/ to 
-- the context.
--
-- This is somewhat contrived, but the bounds and the result
-- event from a PosEvent have to be generated within the same 
-- context.
--
type PosDraw itbl u = u -> CatPrim itbl

type Span u = u


-- | A positionable event.
--
newtype PosEvent itbl ctx u = PosEvent
          { getPosEvent :: Query itbl ctx u (Span u, PosDraw itbl u) }

type instance DUnit (PosEvent itbl ctx u) = u
    
-- | Version of PosObject specialized to Double for the unit type.
--
type DPosEvent itbl ctx = PosEvent itbl ctx Double





--------------------------------------------------------------------------------


-- Monoid
instance (Fractional u, Ord u, InterpretUnit u) => 
    Monoid (PosEvent itbl ctx u) where
  mempty  = emptyPosEvent
  mappend = poconcat


-- | This is /overlay/ concatenation - but events start at the 
-- same time, the result span is the longest.
-- 
poconcat :: (Fractional u, Ord u) 
         => PosEvent itbl ctx u -> PosEvent itbl ctx u -> PosEvent itbl ctx u
poconcat a b = PosEvent body
   where
    body = askCtx >>= \ctx ->
           let a1 = runQuery ctx (getPosEvent a)
               a2 = runQuery ctx (getPosEvent b)
           in pure (appendW a1 a2)



appendW :: (Fractional u, Ord u)
        => (Span u, PosDraw itbl u) 
        -> (Span u, PosDraw itbl u) 
        -> (Span u, PosDraw itbl u)
appendW (s0,pf0) (s1,pf1) = let pf = \pt -> pf0 pt `mappend` pf1 pt
                            in (max s0 s1, pf)



-- | Version of 'runPosEvent' that produces a 
-- 'LocImage' that returns a bounding box. 
-- 
-- The 'PosEvent' is run with only rect-address as an explicit 
-- argument (start-point is implicit). The corresponding answer is 
-- an /arity one/ Graphic that needs drawing with the start-point.
--
runPosEvent :: (Fractional u, InterpretUnit u) 
            => PosEvent itbl ctx u -> LocEvent itbl ctx u (Bounds u)
runPosEvent (PosEvent mf) = promoteLoc $ \ot ->
   askCtx >>= \ctx -> 
   let (s0,df) = runQuery ctx mf
       bb      = Bounds ot (ot + s0)
   in replaceAns bb $ primEvent (df ot)



-- | 'makePosEvent' : @ object_pos * loc_image -> PosEvent @ 
--
-- Create a 'PosEvent' from its length and 'LocEvent'.
--
makePosEvent :: InterpretUnit u
             => Query itbl ctx u (Span u) -> ULocEvent itbl ctx u 
             -> PosEvent itbl ctx u
makePosEvent qortt gf = PosEvent body
  where
    body = askCtx >>= \ctx -> 
           let v1   = runQuery ctx qortt
               pf   = \ot -> getCP $ runLocEvent ot ctx gf
           in return (v1,pf)

    getCP (PrimW ca _) = ca



-- | 'emptyPosEvent' : @ PosEvent @
--
-- Build an empty 'PosGraphicObject'.
--
emptyPosEvent :: InterpretUnit u => PosEvent itbl ctx u
emptyPosEvent = PosEvent $ pure (0, const mempty)

    



-- | Apply a context update to a 'PosEvent'.
--
localPosEvent :: ContextF ctx -> PosEvent itbl ctx u -> PosEvent itbl ctx u
localPosEvent upd = PosEvent . localize upd . getPosEvent


--
-- decorate  - oblivious to /answer/.
-- elaborate - derives annotation from the /answer/ and makes a 
--             cumulative graphic.
--



-- | Note the decoration does not modify the span (duration)
-- even if it may sound for longer.
--
elaboratePosEvent :: InterpretUnit u
                  => (Span u -> ULocEvent itbl ctx u) -> PosEvent itbl ctx u 
                  -> PosEvent itbl ctx u
elaboratePosEvent fn po = PosEvent body
  where
    body = askCtx >>= \ctx -> 
           let (ortt,ptf) = runQuery ctx (getPosEvent po)
               deco       = \pt -> getCP $ runLocEvent pt ctx (fn ortt)
               ptf2       = ptf  `mappend` deco
           in return (ortt, ptf2)

    getCP (PrimW ca _) = ca



-- | Extend the orientation.
--
extendPosEvent :: InterpretUnit u 
               => u -> u -> PosEvent itbl ctx u -> PosEvent itbl ctx u
extendPosEvent t0 t1 po = PosEvent body
  where
    body = askCtx >>= \ctx -> 
           let (b0,pf0) = runQuery ctx (getPosEvent po)
               b1       = t0 + b0 + t1 
               pf1      = \ot -> pf0 (ot + t0) 
           in return (b1,pf1)




--------------------------------------------------------------------------------
-- Combining PosEvent


instance (Fractional u, Ord u, InterpretUnit u) => 
    Overlay (PosEvent itbl ctx u) where
  overlay = mappend



instance Num u => Append (PosEvent itbl ctx u) where
  append = appendPE


appendPE :: (Num u)   
         => PosEvent itbl ctx u -> PosEvent itbl ctx u -> PosEvent itbl ctx u
appendPE pe0 pe1 = PosEvent body
  where
    body = askCtx >>= \ctx -> 
          let (s0,pf0) = runQuery ctx (getPosEvent pe0)
              (s1,pf1) = runQuery ctx (getPosEvent pe1)
              pf       = \ot -> pf0 ot `mappend` (pf1 $ ot + s0)
          in return (s0 + s1, pf)


instance InterpretUnit u => Space (PosEvent itbl ctx u) where
  space = spacePE


spacePE :: InterpretUnit u
        => u -> PosEvent itbl ctx u -> PosEvent itbl ctx u 
        -> PosEvent itbl ctx u
spacePE u a b = a `append` blank `append` b
  where
    blank = makePosEvent (pure u) mempty

