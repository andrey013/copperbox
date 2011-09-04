{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Kernel.Drawing.Chain
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

module Wumpus.Basic.Kernel.Drawing.Chain
  (
  
    GenChain
  , Chain
  , DChain
  , ChainScheme(..)

  , runGenChain
  , evalGenChain
  , execGenChain
  , stripGenChain

  , runChain
  , runChain_

  , onChain
  , setChainScheme

  , chainPrefix

  , chainIterate

  , horizontalChainScm
  , verticalChainScm
  , runChainH
  , runChainV

  , tableRightScm
  , tableDownScm
  
  , runTableRight
  , runTableDown

  , radialChain

  ) where


import Wumpus.Basic.Kernel.Base.BaseDefs
import Wumpus.Basic.Kernel.Base.DrawingContext
import Wumpus.Basic.Kernel.Base.WrappedPrimitive
import Wumpus.Basic.Kernel.Drawing.Basis
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
      { chain_init      :: Point2 u -> cst
      , chain_step      :: Point2 u -> cst -> (Point2 u,cst)
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


-- LocationM

instance InterpretUnit u => LocationM (GenChain st u) where
  location = GenChain $ \ctx pt s ->
      let upt = dinterpF (dc_font_size ctx) pt in (upt, pt, s, mempty) 



-- Monoid

instance Monoid a => Monoid (GenChain st u a) where
  mempty           = GenChain $ \_   pt s -> (mempty, pt, s, mempty)
  ma `mappend` mb  = GenChain $ \ctx pt s -> 
                       let (a,p1,s1,w1) = getGenChain ma ctx pt s
                           (b,p2,s2,w2) = getGenChain mb ctx p1 s1
                       in (a `mappend` b, p2, s2, w1 `mappend` w2)

--------------------------------------------------------------------------------
-- Run functions

runGenChain :: InterpretUnit u 
            => ChainScheme u -> st -> GenChain st u a -> LocImage u (a,st)
runGenChain (ChainScheme start step) ust ma = promoteLoc $ \pt -> 
    askDC >>= \ctx ->
    let st_zero     = ChainSt { chain_st         = start pt
                              , chain_next       = step
                              , chain_user_state = ust }
        dpt         = normalizeF (dc_font_size ctx) pt
        (a,_,s1,w1) = getGenChain ma ctx dpt st_zero
    in replaceAns (a, chain_user_state s1) $ primGraphic w1



-- | Forget the user state LocImage, just return the /answer/.
--
evalGenChain :: InterpretUnit u 
             => ChainScheme u -> st -> GenChain st u a -> LocImage u a
evalGenChain cscm st ma = fmap fst $ runGenChain cscm st ma


-- | Forget the /answer/, just return the user state.
--
execGenChain :: InterpretUnit u 
             => ChainScheme u -> st -> GenChain st u a -> LocImage u st 
execGenChain cscm st ma = fmap snd $ runGenChain cscm st ma


stripGenChain :: InterpretUnit u 
              => ChainScheme u -> st -> GenChain st u a -> LocQuery u (a,st)
stripGenChain cscm st ma = stripLocImage $ runGenChain cscm st ma



runChain :: InterpretUnit u 
         => ChainScheme u -> Chain u a -> LocImage u a
runChain cscm ma = evalGenChain cscm () ma

runChain_ :: InterpretUnit u 
          => ChainScheme u -> Chain u a -> LocGraphic u
runChain_ cscm ma = ignoreAns $ runChain cscm ma




--------------------------------------------------------------------------------
-- Operations

onChain :: InterpretUnit u 
        => LocImage u a -> GenChain st u a
onChain gf  = GenChain $ \ctx pt (ChainSt s0 sf ust) -> 
    let upt       = dinterpF (dc_font_size ctx) pt
        (a,w1)    = runImage ctx $ applyLoc gf upt
        (pt1,st1) = sf upt s0
        dpt1      = normalizeF (dc_font_size ctx) pt1
        new_st    = ChainSt { chain_st         = st1
                            , chain_next       = sf
                            , chain_user_state = ust }
    in (a, dpt1, new_st, w1)

--
-- Note - onChain draws at the initial position, then increments 
-- the next position.
--


setChainScheme :: InterpretUnit u 
               => ChainScheme u -> GenChain st u ()
setChainScheme (ChainScheme start step) = 
    GenChain $ \ctx pt (ChainSt _ _ ust) -> 
      let upt     = dinterpF (dc_font_size ctx) pt
          new_st  = ChainSt { chain_st         = start upt
                            , chain_next       = step
                            , chain_user_state = ust }
      in ((), pt, new_st, mempty) 



chainPrefix :: ChainScheme u -> Int -> ChainScheme u -> ChainScheme u
chainPrefix (ChainScheme astart astep) ntimes chb@(ChainScheme bstart bstep)
    | ntimes < 1 = chb
    | otherwise  = ChainScheme { chain_init = start, chain_step = next }
  where
    start pt = (astart pt,ntimes, bstart pt)

    next pt (ast,n,bst) 
        | n > 0     = let (p2,ast1) = astep pt ast in (p2, (ast1,n-1,bst))
        | n == 0    = let bst1      = bstart pt 
                          (p2,bst2) = bstep pt bst1 
                      in (p2, (ast,(-1),bst2))
        | otherwise = let (p2,bst1) = bstep pt bst in (p2,(ast, (-1), bst1))
 




--------------------------------------------------------------------------------
-- Schemes

chainIterate :: (Point2 u -> Point2 u) -> ChainScheme u
chainIterate fn = ChainScheme { chain_init = const ()
                              , chain_step = \pt _ -> (fn pt, ())
                              }


horizontalChainScm :: Num u => u -> ChainScheme u
horizontalChainScm dx = 
    ChainScheme { chain_init = const ()
                , chain_step = \pt _ -> (displace (hvec dx) pt, ())
                }
   
verticalChainScm :: Num u => u -> ChainScheme u
verticalChainScm dy = 
    ChainScheme { chain_init = const ()
                , chain_step = \pt _ -> (displace (vvec dy) pt, ())
                }


-- Horizontal and vertical chains are common enough to merit 
-- dedicated run functions.

runChainH :: InterpretUnit u => u -> Chain u a -> LocImage u a
runChainH dx ma = runChain (horizontalChainScm dx) ma


runChainV :: InterpretUnit u => u -> Chain u a -> LocImage u a
runChainV dy ma = runChain (verticalChainScm dy) ma


-- | Outer and inner steppers.
--
scStepper :: PointDisplace u -> Int -> PointDisplace u 
          -> ChainScheme u
scStepper outF n innF = 
    ChainScheme { chain_init = start, chain_step = step }
  where
    start pt                      = (pt,1)
    step  pt (ogin,i) | i <  n    = (innF pt, (ogin, i+1))
                      | otherwise = let o1 = outF ogin 
                                    in (o1, (o1,1)) 


tableRightScm :: Num u => Int -> (u,u) -> ChainScheme u
tableRightScm num_cols (col_width,row_height) = 
    scStepper downF num_cols rightF
  where
    downF   = displace $ vvec $ negate row_height
    rightF  = displace $ hvec col_width

tableDownScm :: Num u => Int -> (u,u) -> ChainScheme u
tableDownScm num_rows (col_width,row_height) = 
    scStepper rightF num_rows downF
  where
    downF   = displace $ vvec $ negate row_height
    rightF  = displace $ hvec col_width


runTableRight :: InterpretUnit u 
             => Int -> (u,u) -> Chain u a -> LocImage u a
runTableRight num_cols dims ma = 
    runChain (tableRightScm num_cols dims) ma


runTableDown :: InterpretUnit u 
             => Int -> (u,u) -> Chain u a -> LocImage u a
runTableDown num_rows dims ma = 
    runChain (tableDownScm num_rows dims) ma



radialChain :: Floating u 
            => u -> Radian -> Radian -> ChainScheme u
radialChain radius angstart angi = 
    ChainScheme { chain_init = start, chain_step = step }
  where
    start pt           = let ogin = displace (avec angstart (-radius)) pt
                         in (ogin, angstart)
    step  _ (ogin,ang) = let ang_next = ang + angi 
                             pt       = displace (avec ang_next radius) ogin
                         in (pt, (ogin, ang_next))

    

-- radialChain is convoluted because first point is not the 
-- circle center but a point on the circumference. Also the next
-- step iterates the (constant) origin rather than the previous 
-- point.


-- Note - radialChains stepper is oblivious to the previous point...


