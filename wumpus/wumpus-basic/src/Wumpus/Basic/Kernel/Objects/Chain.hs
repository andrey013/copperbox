{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Kernel.Objects.Chain
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  highly unstable
-- Portability :  GHC 
--
-- Chaining moveable LocGraphics.
--
--------------------------------------------------------------------------------

module Wumpus.Basic.Kernel.Objects.Chain
  (
  
    GenChain
  , Chain
  , DChain
  , ChainScheme(..)

  , runChain
  , runChain_

  , cnext
  , setChainScheme


  , chainIterate
  , chainH
  , chainV

  , tableRight  
  , tableDown

  , radialChain

  ) where


import Wumpus.Basic.Kernel.Base.BaseDefs
import Wumpus.Basic.Kernel.Base.DrawingContext
import Wumpus.Basic.Kernel.Base.UserState
import Wumpus.Basic.Kernel.Base.WrappedPrimitive
import Wumpus.Basic.Kernel.Objects.Basis
import Wumpus.Basic.Kernel.Objects.Displacement
import Wumpus.Basic.Kernel.Objects.Image
import Wumpus.Basic.Kernel.Objects.LocImage

import Wumpus.Core                              -- package: wumpus-core

import Control.Applicative
import Data.Monoid



newtype GenChain st u a = GenChain
          { getGenChain :: DrawingContext -> DPoint2 -> ChainSt st u 
                        -> (a, DPoint2, ChainSt st u, CatPrim) }


type instance DUnit (GenChain st u a) = u
type instance UState  (GenChain st u) = st

type Chain u a   = GenChain () u a

type DChain a    = Chain Double a

-- | scheme_start is a function from the origin to state.
-- 
-- For instance, we might want to cache the origin - this would
-- not be possible if start was just a pure @cst@ value. 
--
data ChainScheme u = forall cst. ChainScheme 
      { scheme_start    :: Point2 u -> cst
      , scheme_step     :: Point2 u -> cst -> (Point2 u,cst)
      }

type instance DUnit (ChainScheme u) = u


data ChainSt st u = forall cst. ChainSt 
       { chain_st         :: cst
       , chain_next       :: Point2 u -> cst -> (Point2 u,cst) 
       , chain_user_state :: st
       }


type instance DUnit (ChainSt st u) = u


-- Functor 

instance Functor (GenChain st u) where
  fmap f ma = GenChain $ \ctx pt s -> 
              let (a,p1,s1,w) = getGenChain ma ctx pt s in (f a, p1, s1, w)



-- Applicative

instance Applicative (GenChain st u) where
  pure a    = GenChain $ \_   pt s -> (a, pt, s, mempty)
  mf <*> ma = GenChain $ \ctx pt s -> 
                let (f,p1,s1,w1) = getGenChain mf ctx pt s
                    (a,p2,s2,w2) = getGenChain ma ctx p1 s1
                in (f a, p2, s2, w1 `mappend` w2)



-- Monad

instance Monad (GenChain st u) where
  return a  = GenChain $ \_   pt s -> (a, pt, s, mempty)
  ma >>= k  = GenChain $ \ctx pt s -> 
                let (a,p1,s1,w1) = getGenChain ma ctx pt s
                    (b,p2,s2,w2) = (getGenChain . k) a ctx p1 s1
                in (b, p2, s2, w1 `mappend` w2)


-- DrawingCtxM

instance DrawingCtxM (GenChain st u) where
  askDC           = GenChain $ \ctx pt s -> (ctx, pt, s, mempty)
  asksDC fn       = GenChain $ \ctx pt s -> (fn ctx, pt, s, mempty)
  localize upd ma = GenChain $ \ctx pt s -> getGenChain ma (upd ctx) pt s



-- UserStateM 

instance UserStateM (GenChain st u) where
  getState        = GenChain $ \_ pt s@(ChainSt _ _ ust) -> 
                      (ust, pt, s, mempty)
  setState ust    = GenChain $ \_ pt (ChainSt a b _) -> 
                      ((), pt, ChainSt a b ust, mempty)
  updateState upd = GenChain $ \_ pt (ChainSt a b ust) -> 
                      ((), pt, ChainSt a b (upd ust), mempty)



-- Monoid

instance Monoid a => Monoid (GenChain st u a) where
  mempty           = GenChain $ \_   pt s -> (mempty, pt, s, mempty)
  ma `mappend` mb  = GenChain $ \ctx pt s -> 
                       let (a,p1,s1,w1) = getGenChain ma ctx pt s
                           (b,p2,s2,w2) = getGenChain mb ctx p1 s1
                       in (a `mappend` b, p2, s2, w1 `mappend` w2)


runGenChain :: InterpretUnit u 
         => GenChain st u a -> ChainScheme u -> st -> LocImage u a
runGenChain ma (ChainScheme start step) ust = promoteLoc $ \pt -> 
    askDC >>= \ctx ->
    let st_zero    = ChainSt { chain_st         = start pt
                             , chain_next       = step
                             , chain_user_state = ust }
        dpt        = normalizeF (dc_font_size ctx) pt
        (a,_,_,w1) = getGenChain ma ctx dpt st_zero
    in replaceAns a $ primGraphic w1



runChain :: InterpretUnit u 
         => Chain u a -> ChainScheme u -> LocImage u a
runChain ma cscm = runGenChain ma cscm ()

runChain_ :: InterpretUnit u 
          => Chain u a -> ChainScheme u -> LocGraphic u
runChain_ ma cscm = ignoreAns $ runChain ma cscm


cnext :: InterpretUnit u 
      => LocImage u a -> GenChain st u a
cnext gf  = GenChain $ \ctx pt (ChainSt s0 sf ust) -> 
    let dpt       = dinterpF (dc_font_size ctx) pt
        (pt1,st1) = sf dpt s0
        dpt1      = normalizeF (dc_font_size ctx) pt1
        (a,w1)    = runImage (applyLoc gf pt1) ctx
        new_st    = ChainSt { chain_st = st1
                            , chain_next = sf
                            , chain_user_state = ust }
    in (a, dpt1, new_st, w1)


setChainScheme :: InterpretUnit u 
               => ChainScheme u -> GenChain st u ()
setChainScheme (ChainScheme start step) = 
    GenChain $ \ctx pt (ChainSt _ _ ust) -> 
      let upt     = dinterpF (dc_font_size ctx) pt
          new_st  = ChainSt { chain_st = start upt
                            , chain_next = step
                            , chain_user_state = ust }
      in ((), pt, new_st, mempty) 



--------------------------------------------------------------------------------
-- Schemes

chainIterate :: (Point2 u -> Point2 u) -> ChainScheme u
chainIterate fn = ChainScheme { scheme_start = const ()
                              , scheme_step  = \pt _ -> (fn pt, ())
                              }


chainH :: Num u => u -> ChainScheme u
chainH dx = 
    ChainScheme { scheme_start = const ()
                , scheme_step  = \pt _ -> (displace (hvec dx) pt, ())
                }
   
chainV :: Num u => u -> ChainScheme u
chainV dy = 
    ChainScheme { scheme_start = const ()
                , scheme_step  = \pt _ -> (displace (vvec dy) pt, ())
                }




-- | Outer and inner steppers.
--
scStepper :: PointDisplace u -> Int -> PointDisplace u 
          -> ChainScheme u
scStepper outF n innF = 
    ChainScheme { scheme_start = start, scheme_step = step }
  where
    start pt                      = (pt,0)
    step  pt (ogin,i) | i < n     = (innF pt, (ogin, i+1))
                      | otherwise = let o1 = outF ogin 
                                    in (innF o1, (o1,1)) 


tableRight :: Num u => Int -> (u,u) -> ChainScheme u
tableRight num_cols (col_width,row_height) = 
    scStepper downF num_cols rightF
  where
    downF   = displace $ vvec $ negate row_height
    rightF  = displace $ hvec col_width

tableDown :: Num u => Int -> (u,u) -> ChainScheme u
tableDown num_rows (col_width,row_height) = 
    scStepper rightF num_rows downF
  where
    downF   = displace $ vvec $ negate row_height
    rightF  = displace $ hvec col_width



radialChain :: Floating u 
            => u -> Radian -> Radian -> ChainScheme u
radialChain radius angstart angi = 
    ChainScheme { scheme_start = start, scheme_step = step }
  where
    start pt           = (pt,angstart)
    step  _ (ogin,ang) = (displace (avec ang radius) ogin, (ogin,ang + angi))

-- Note - radialChains stepper is oblivious to the previous point...


