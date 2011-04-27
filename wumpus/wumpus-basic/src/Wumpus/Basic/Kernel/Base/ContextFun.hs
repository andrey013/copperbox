{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Kernel.Base.ContextFun
-- Copyright   :  (c) Stephen Tetley 2010-2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  highly unstable
-- Portability :  GHC 
--
-- Function types operating over the DrawingContext as a /static/ 
-- argument. 
--
--------------------------------------------------------------------------------

module Wumpus.Basic.Kernel.Base.ContextFun
  (

  -- * /Context functional/ type
    CF     

  -- * Alias
  , Query

  -- * Run function
  , runCF

  -- * Construction
  , consCF

  , domMap1
  , domMap2

  , promoteR1
  , promoteR2

  , pushR0
  , pushR1
  , pushR2

  , apply1R1
  , apply1R2
  , apply2R2

  , lift0R1
  , lift0R2
  , lift1R2

  , uconvR0
  , uconvR1
  , uconvR2a
  , uconvR2ab

  , drawingCtx

  ) where

import Wumpus.Basic.Kernel.Base.BaseDefs
import Wumpus.Basic.Kernel.Base.DrawingContext

import Wumpus.Core                              -- package: wumpus-core


import Control.Applicative
import Data.Monoid


--------------------------------------------------------------------------------
--

-- NOTE - 31.03.11 - is there an advantage to adding a phantom
-- unit param to the CF functions?

-- | Most drawing operations in Wumpus-Basic have an implicit 
-- /graphics state/ the 'DrawingContext', so the most primitive 
-- building block is a function from the DrawingContext to some 
-- polymorphic answer.
-- 
-- This functional type is represented concretely as the initials 
-- @CF@ for /contextual function/.
-- 
-- > CF :: DrawingContext -> a 
--
newtype CF a            = CF  { getCF :: DrawingContext -> a }

type instance DUnit (CF a) = DUnit a



-- | Alias for 'CF'. Wumpus considers Context functions that
-- don\'t produce graphics to be /queries/.
-- 
-- > Query :: DrawingContext -> a 
--
-- 'Query' has no unit type parameter.
-- 
type Query a            = CF a
 





--------------------------------------------------------------------------------
-- CF instances

-- OPlus

instance OPlus a => OPlus (CF a)  where
  fa `oplus` fb = CF $ \ctx -> getCF fa ctx `oplus` getCF fb ctx


-- | Monoid
--
-- Note - the mconcat definition avoids starting with an initial 
-- mempty. This contrasts with the default definition in the 
-- Monoid class that uses foldr.
--
instance Monoid a => Monoid (CF a) where 
  mempty          = CF $ \_   -> mempty
  fa `mappend` fb = CF $ \ctx -> getCF fa ctx `mappend` getCF fb ctx

  mconcat []      = mempty
  mconcat (a:as)  = step a as
    where
      step ac []     = ac
      step ac (x:xs) = step (ac `mappend` x) xs

-- Functor

instance Functor CF where
  fmap f ma = CF $ \ctx -> f $ getCF ma ctx 



-- Applicative

instance Applicative CF where
  pure a    = CF $ \_   -> a
  mf <*> ma = CF $ \ctx -> let f = getCF mf ctx
                               a = getCF ma ctx
                           in f a



-- Monad 

instance Monad CF where
  return a  = CF $ \_   -> a
  ma >>= k  = CF $ \ctx -> let a = getCF ma ctx in (getCF . k) a ctx 


-- DrawingCtxM 

instance DrawingCtxM CF where
  askDC           = CF $ \ctx -> ctx
  localize upd df = CF $ \ctx -> getCF df (upd ctx)
  


--------------------------------------------------------------------------------
-- Affine instances



instance Rotate a => Rotate (CF a) where
  rotate ang            = fmap (rotate ang)

instance RotateAbout a => RotateAbout (CF a) where
  rotateAbout ang pt    = fmap (rotateAbout ang pt)

instance Scale a => Scale (CF a) where
  scale sx sy           = fmap (scale sx sy)

instance Translate a => Translate (CF a) where
  translate dx dy       = fmap (translate dx dy)



--------------------------------------------------------------------------------
-- Run functions

-- | Run a /CF/ (context function) with the supplied 
-- /DrawingContext/.
--
runCF :: DrawingContext -> CF a -> a
runCF ctx cf = getCF cf ctx



-- | Construction function.
--
consCF :: (DrawingContext -> a) -> CF a
consCF fn = CF $ \ctx -> fn ctx





domMap1 :: (r1 -> r1) -> CF (r1 -> a) -> CF (r1 -> a)
domMap1 f ma = CF $ \ctx -> 
    (\r1 -> let f1 = getCF ma ctx in f1 (f r1))

domMap2 :: (r1 -> r1) -> (r2 -> r2) -> CF (r1 -> r2 -> a) -> CF (r1 -> r2 -> a)
domMap2 f g ma = CF $ \ctx -> 
    (\r1 r2 -> let f1 = getCF ma ctx in f1 (f r1) (g r2))



promoteR1 :: (r1 -> CF a) -> CF (r1 -> a)
promoteR1 mf = CF $ \ctx r1 -> getCF (mf r1) ctx

promoteR2 :: (r1 -> r2 -> CF a) -> CF (r1 -> r2 -> a)
promoteR2 mf = CF $ \ctx r1 r2 -> getCF (mf r1 r2) ctx

-- | Apply the value transformer to the answer of the context 
-- function. Figuratively /push it right/ so it works on the 
-- answer.
--
-- > pushR0 = fmap
--
pushR0 :: (a -> a1) -> CF a -> CF a1
pushR0 = fmap


-- | Apply the value transformer to the answer of the context 
-- function. Figuratively /push it right/ so it works on the 
-- answer.
--
-- > pushR1 = fmap . fmap
--
pushR1 :: (a -> a1) -> CF (r1 -> a) -> CF (r1 -> a1)
pushR1 = fmap . fmap

-- | Apply the value transformer to the answer of the context 
-- function. Figuratively /push it right/ so it works on the 
-- answer.
--
-- > pushR2 = fmap . fmap . fmap 
--
pushR2 :: (a -> a1) -> CF (r1 -> r2 -> a) -> CF (r1 -> r2 -> a1)
pushR2 = fmap . fmap . fmap




apply1R1 :: CF (r1 -> a) -> r1 -> CF a
apply1R1 mf r1 = CF $ \ctx -> getCF mf ctx r1


apply1R2 :: CF (r1 -> r2 -> a) -> r2 -> CF (r1 -> a)
apply1R2 mf r2 = CF $ \ctx r1 -> getCF mf ctx r1 r2


apply2R2 :: CF (r1 -> r2 -> a) -> r1 -> r2 -> CF a
apply2R2 mf r1 r2 = CF $ \ctx -> getCF mf ctx r1 r2


lift0R1             :: CF a -> CF (r1 -> a)
lift0R1 mf          = CF $ \ctx _ -> getCF mf ctx

lift0R2             :: CF a -> CF (r1 -> r2 -> a)
lift0R2 mf          = CF $ \ctx _ _ -> getCF mf ctx

lift1R2             :: CF (r1 -> a) -> CF (r1 -> r2 -> a)
lift1R2 mf          = CF $ \ctx r1 _ -> getCF mf ctx r1




uconvR0 :: (FontSize -> ans -> ans1)  -> CF ans -> CF ans1
uconvR0 post df = CF $ \ctx ->
    let sz = dc_font_size ctx 
    in post sz $ getCF df ctx


uconvR1 :: (Functor t, InterpretUnit u, InterpretUnit u1) 
        => (FontSize -> ans -> ans1) 
        -> CF (t u -> ans) -> CF (t u1 -> ans1)
uconvR1 post df = CF $ \ctx r1 -> 
    let sz = dc_font_size ctx 
    in post sz $ getCF df ctx (uconvertF sz r1)


uconvR2a :: (Functor t, InterpretUnit u, InterpretUnit u1) 
         => (FontSize -> ans -> ans1)
         -> CF (t u -> r2 -> ans) -> CF (t u1 -> r2 -> ans1)
uconvR2a post df = CF $ \ctx r1 r2 -> 
    let sz = dc_font_size ctx 
    in post sz $ getCF df ctx (uconvertF sz r1) r2



uconvR2ab :: (Functor t1, Functor t2, InterpretUnit u, InterpretUnit u1) 
          => (FontSize -> ans -> ans1)
          -> CF (t1 u -> t2 u ->  ans) -> CF (t1 u1 -> t2 u1 -> ans1)
uconvR2ab post df = CF $ \ctx r1 r2 -> 
    let sz = dc_font_size ctx 
    in post sz $ getCF df ctx (uconvertF sz r1) (uconvertF sz r2)



-- | Extract the drawing context from a CtxFun.
--
-- > (ctx -> ctx)
-- 
drawingCtx      :: Query DrawingContext
drawingCtx      = CF $ \ctx -> ctx


