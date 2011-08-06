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

    Chain
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
import Wumpus.Basic.Kernel.Base.WrappedPrimitive
import Wumpus.Basic.Kernel.Objects.Basis
import Wumpus.Basic.Kernel.Objects.Displacement
import Wumpus.Basic.Kernel.Objects.LocImage

import Wumpus.Core                              -- package: wumpus-core

import Control.Applicative
import Data.Monoid



newtype Chain u a = Chain
          { getChain :: DrawingContext -> DPoint2 -> ChainSt u 
                     -> (a, DPoint2, ChainSt u, CatPrim) }


type instance DUnit (Chain u a) = u


type DChain a    = Chain Double a

-- | scheme_start is a function from the origin to state.
-- 
-- For instance, we might want to cache the origin - this would
-- not be possible if start was just a pure @st@ value. 
--
data ChainScheme u = forall st. ChainScheme 
      { scheme_start    :: Point2 u -> st
      , scheme_step     :: Point2 u -> st -> (Point2 u,st)
      }

type instance DUnit (ChainScheme u) = u


data ChainSt u = forall st. ChainSt 
       { chain_st   :: st
       , chain_next :: Point2 u -> st -> (Point2 u,st) 
       }


type instance DUnit (ChainSt u) = u


-- Functor 

instance Functor (Chain u) where
  fmap f ma = Chain $ \r s t -> let (a,s1,t1,w) = getChain ma r s t
                                in (f a, s1, t1, w)



-- Applicative

instance Applicative (Chain u) where
  pure a    = Chain $ \_ s t -> (a, s, t, mempty)
  mf <*> ma = Chain $ \r s t -> 
                let (f,s1,t1,w1) = getChain mf r s t
                    (a,s2,t2,w2) = getChain ma r s1 t1
                in (f a, s2, t2, w1 `mappend` w2)



-- Monad

instance Monad (Chain u) where
  return a  = Chain $ \_ s t -> (a, s, t, mempty)
  ma >>= k  = Chain $ \r s t -> 
                let (a,s1,t1,w1) = getChain ma r s t
                    (b,s2,t2,w2) = (getChain . k) a r s1 t1
                in (b, s2, t2, w1 `mappend` w2)


instance DrawingCtxM (Chain u) where
  askDC           = Chain $ \r s t -> (r, s, t, mempty)
  asksDC fn       = Chain $ \r s t -> (fn r, s, t, mempty)
  localize upd ma = Chain $ \r s t -> getChain ma (upd r) s t



-- Monoid

instance Monoid a => Monoid (Chain u a) where
  mempty           = Chain $ \_ s t -> (mempty, s, t, mempty)
  ma `mappend` mb  = Chain $ \r s t -> 
                       let (a,s1,t1,w1) = getChain ma r s t
                           (b,s2,t2,w2) = getChain mb r s1 t1
                       in (a `mappend` b, s2, t2, w1 `mappend` w2)


runChain :: InterpretUnit u 
         => ChainScheme u -> Chain u a -> LocImage u a
runChain (ChainScheme start step) ma = promoteLoc $ \pt -> 
    askDC >>= \ctx ->
    let st_zero    = ChainSt { chain_st   = start pt
                             , chain_next = step }
        dpt        = normalizeF (dc_font_size ctx) pt
        (a,_,_,w1) = getChain ma ctx dpt st_zero
    in replaceAns a $ primGraphic w1


runChain_ :: InterpretUnit u 
          => ChainScheme u -> Chain u a -> LocGraphic u
runChain_ cscm ma = ignoreAns $ runChain cscm ma


cnext :: InterpretUnit u 
      => LocImage u a -> Chain u a
cnext gf  = Chain $ \ctx pt (ChainSt s0 sf) -> 
    let dpt       = dinterpF (dc_font_size ctx) pt
        (pt1,st1) = sf dpt s0
        dpt1      = normalizeF (dc_font_size ctx) pt1
        (a,w1)    = runImage ctx (applyLoc gf pt1)
    in (a, dpt1, ChainSt { chain_st = st1, chain_next = sf }, w1)


setChainScheme :: InterpretUnit u 
               => ChainScheme u -> Chain u ()
setChainScheme (ChainScheme start step) = Chain $ \ctx pt _ -> 
    let upt = dinterpF (dc_font_size ctx) pt
        st  = ChainSt { chain_st = start upt
                      , chain_next = step }
    in ((), pt, st, mempty) 



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


