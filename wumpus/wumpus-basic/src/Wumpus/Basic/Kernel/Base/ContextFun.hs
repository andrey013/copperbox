{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Kernel.Base.ContextFun
-- Copyright   :  (c) Stephen Tetley 2010
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

  -- * /Context functional/ types
    CF     
  , CF1
  , CF2

  , LocCF
  , LocThetaCF
  , ConnectorCF
  , DLocCF
  , DLocThetaCF
  , DConnectorCF



  -- * Run functions
  , runCF
  , runCF1 
  , runCF2

  -- * Lift functions
  , lift0R1
  , lift0R2
  , lift1R2
 
  , promoteR1
  , promoteR2


  , apply1R1
  , apply2R2
  , apply1R2

  -- * Extractors
  , drawingCtx
  , queryCtx
  , locCtx
  , locPoint
  , locThetaCtx
  , locThetaPoint
  , locThetaAng
  , connCtx
  , connStart
  , connEnd

  -- * Combinators
  , at
  , chain1


  ) where

import Wumpus.Basic.Kernel.Base.BaseDefs
import Wumpus.Basic.Kernel.Base.DrawingContext

import Wumpus.Core                              -- package: wumpus-core


import Control.Applicative
import Data.Monoid


--------------------------------------------------------------------------------
--

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
newtype CF a            = CF  { unCF :: DrawingContext -> a }


-- | Variation of 'CF' with one parametric /static argument/.
--
-- The static argument is commonly a point representing the start 
-- point / origin of a drawing.
-- 
-- > CF1 :: DrawingContext -> r1 -> a 
--
newtype CF1 r1 a        = CF1 { unCF1 :: DrawingContext -> r1 -> a }


-- | Variation of 'CF' with two parametric /static arguments/.
--
-- The first argument is commonly a point representing the start 
-- point / origin of a drawing. The second argument might 
-- typically be the angle of displacement (for drawing arrowheads) 
-- or an end point (for drawing connectors between two points).
-- 
-- > CF2 :: DrawingContext -> r1 -> r2 -> a 
--
newtype CF2 r1 r2 a     = CF2 { unCF2 :: DrawingContext -> r1 -> r2 -> a }

 

-- | Type specialized verison of 'CF1' where the /static argument/
-- is the /start point/.
-- 
-- > LocCF :: DrawingContext -> Point2 u -> a 
--
type LocCF          u a = CF1 (Point2 u) a


-- | Type specialized verison of 'CF2' where the /static arguments/
-- are the /start point/ and the /angle of displacement/.
-- 
-- > LocThetaCF :: DrawingContext -> Point2 u -> Radian -> a 
--
type LocThetaCF     u a = CF2 (Point2 u) Radian a


-- | Type specialized verison of 'CF2' where the /static arguments/
-- are the /start point/ and the /end point/.
-- 
-- > ConnectorCF :: DrawingContext -> Point2 u -> Point2 u -> a 
--
type ConnectorCF    u a = CF2 (Point2 u) (Point2 u) a


-- | Alias of 'LocCF' where the unit type is specialized to 
-- @Double@.
--
type DLocCF a           = LocCF       Double a


-- | Alias of 'LocThetaCF' where the unit type is specialized to 
-- @Double@.
--
type DLocThetaCF a      = LocThetaCF  Double a

-- | Alias of 'ConnectorCF' where the unit type is specialized to 
-- @Double@.
--
type DConnectorCF a     = ConnectorCF Double a



--------------------------------------------------------------------------------
-- CF instances

-- OPlus

instance OPlus a => OPlus (CF a)  where
  fa `oplus` fb = CF $ \ctx -> unCF fa ctx `oplus` unCF fb ctx

instance OPlus a => OPlus (CF1 r1 a)  where
  fa `oplus` fb = CF1 $ \ctx r1 -> unCF1 fa ctx r1 `oplus` unCF1 fb ctx r1

instance OPlus a => OPlus (CF2 r1 r2 a)  where
  fa `oplus` fb = CF2 $ \ctx r1 r2 -> 
                          unCF2 fa ctx r1 r2 `oplus` unCF2 fb ctx r1 r2


-- Monoid
 
-- Nothing is stopping monoid instances, though in practice there
-- might be few useful types (more in Semgigroup / OPlus)...

instance Monoid a => Monoid (CF a) where 
  mempty          = CF $ \_   -> mempty
  fa `mappend` fb = CF $ \ctx -> unCF fa ctx `mappend` unCF fb ctx

instance Monoid a => Monoid (CF1 r1 a) where 
  mempty          = CF1 $ \_   _  -> mempty
  fa `mappend` fb = CF1 $ \ctx r1 -> unCF1 fa ctx r1 `mappend` unCF1 fb ctx r1

instance Monoid a => Monoid (CF2 r1 r2 a) where 
  mempty          = CF2 $ \_   _  _  -> mempty
  fa `mappend` fb = CF2 $ \ctx r1 r2 -> 
                            unCF2 fa ctx r1 r2 `mappend` unCF2 fb ctx r1 r2



-- Functor

instance Functor CF where
  fmap f ma = CF $ \ctx -> f $ unCF ma ctx 


instance Functor (CF1 r1) where
  fmap f ma = CF1 $ \ctx r1 -> f $ unCF1 ma ctx r1 

instance Functor (CF2 r1 r2) where
  fmap f ma = CF2 $ \ctx r1 r2 -> f $ unCF2 ma ctx r1 r2



-- Applicative

instance Applicative CF where
  pure a    = CF $ \_   -> a
  mf <*> ma = CF $ \ctx -> let f = unCF mf ctx
                               a = unCF ma ctx
                           in f a


instance Applicative (CF1 r1) where
  pure a    = CF1 $ \_   _  -> a
  mf <*> ma = CF1 $ \ctx r1 -> let f = unCF1 mf ctx r1 
                                   a = unCF1 ma ctx r1
                               in f a


instance Applicative (CF2 r1 r2) where
  pure a    = CF2 $ \_   _  _  -> a
  mf <*> ma = CF2 $ \ctx r1 r2 -> let f = unCF2 mf ctx r1 r2
                                      a = unCF2 ma ctx r1 r2
                                  in f a




-- Monad 

instance Monad CF where
  return a  = CF $ \_   -> a
  ma >>= k  = CF $ \ctx -> let a = unCF ma ctx in (unCF . k) a ctx 

instance Monad (CF1 r1) where
  return a  = CF1 $ \_   _  -> a
  ma >>= k  = CF1 $ \ctx r1 -> let a = unCF1 ma ctx  r1 in (unCF1 . k) a ctx r1 

instance Monad (CF2 r1 r2) where
  return a  = CF2 $ \_   _  _  -> a
  ma >>= k  = CF2 $ \ctx r1 r2 -> 
                      let a = unCF2 ma ctx r1 r2 in (unCF2 . k) a ctx r1 r2



-- DrawingCtxM 

instance DrawingCtxM CF where
  askDC           = CF $ \ctx -> ctx
  localize upd df = CF $ \ctx -> unCF df (upd ctx)
  

instance DrawingCtxM (CF1 r1) where
  askDC           = CF1 $ \ctx _  -> ctx
  localize upd df = CF1 $ \ctx r1 -> unCF1 df (upd ctx) r1


instance DrawingCtxM (CF2 r1 r2) where
  askDC           = CF2 $ \ctx _  _  -> ctx
  localize upd df = CF2 $ \ctx r1 r2 -> unCF2 df (upd ctx) r1 r2


-- Note - there is nothing determining a DUnit for the CF types, 
-- so it seems appropriate not to define affine instances.
--
-- However affine instances can be made for the Image type in 
-- Objects.BaseObjects.
--


--------------------------------------------------------------------------------
-- Run functions

-- | Run a /CF/ (context function) with the supplied 
-- /DrawingContext/.
--
runCF :: DrawingContext -> CF a -> a
runCF ctx df = unCF df ctx


-- | Run a /CF1/ (context function) with the supplied 
-- /DrawingContext/ and static argument.
--
runCF1 :: DrawingContext -> r1 -> CF1 r1 a -> a
runCF1 ctx r1 df = unCF1 df ctx r1


-- | Run a /CF1/ (context function) with the supplied 
-- /DrawingContext/ and two static arguments.
--
runCF2 :: DrawingContext -> r1 -> r2 -> CF2 r1 r2 a -> a
runCF2 ctx r1 r2 df = unCF2 df ctx r1 r2



--------------------------------------------------------------------------------
-- lift functions


-- | Lift a zero-arity context function 'CF' to an arity one 
-- context function 'CF1'.
-- 
lift0R1             :: CF a -> CF1 r1 a
lift0R1 mf          = CF1 $ \ctx _ -> unCF mf ctx

-- | Lift a zero-arity context function 'CF' to an arity two 
-- context function 'CF2'.
-- 
lift0R2             :: CF a -> CF2 r1 r2 a
lift0R2 mf          = CF2 $ \ctx _ _ -> unCF mf ctx

-- | Lift an arity one context function 'CF1' to an arity two
-- context function 'CF2'.
-- 
lift1R2             :: CF1 r1 a -> CF2 r1 r2 a
lift1R2 mf          = CF2 $ \ctx r1 _ -> unCF1 mf ctx r1



-- | Promote a function @from one argument to a Context function@ 
-- to an arity one @Context function@.
--
-- The type signature is as explanatory as a description:
--
-- > promoteR1 :: (r1 -> CF a) -> CF1 r1 a
-- 
promoteR1           :: (r1 -> CF a) -> CF1 r1 a
promoteR1 mf        = CF1 $ \ctx r1 -> unCF (mf r1) ctx

-- | Promote a function @from two arguments to a Context function@ 
-- to an arity two @Context function@.
--
-- The type signature is as explanatory as a description:
--
-- > promoteR2 :: (r1 -> r2 -> CF a) -> CF2 r1 r2 a
-- 
promoteR2           :: (r1 -> r2 -> CF a) -> CF2 r1 r2 a
promoteR2 mf        = CF2 $ \ctx r1 r2 -> unCF (mf r1 r2) ctx



-- | Apply an arity-one Context function to a single argument, 
-- downcasting it by one level, making an arity-zero Context 
-- function. 
-- 
-- The type signature is as explanatory as a description:
--
-- > apply1R1 :: CF1 r1 a -> r1 -> CF a
--
apply1R1            :: CF1 r1 a -> r1 -> CF a
apply1R1 mf r1      = CF $ \ctx -> unCF1 mf ctx r1


-- | Apply an arity-two Context function to two arguments, 
-- downcasting it by two levels, making an arity-zero Context 
-- function. 
-- 
-- The type signature is as explanatory as a description:
--
-- > apply2R2 :: CF2 r1 r2 a -> r1 -> r2 -> CF a
-- 
apply2R2            :: CF2 r1 r2 a -> r1 -> r2 -> CF a
apply2R2 mf r1 r2   = CF $ \ctx -> unCF2 mf ctx r1 r2

-- | Apply an arity-two Context function to one argument, 
-- downcasting it by one level, making an arity-one Context 
-- function. 
-- 
-- The type signature is as explanatory as a description:
--
-- > apply1R2 :: CF2 r1 r2 a -> r2 -> CF1 r1 a
--
apply1R2            :: CF2 r1 r2 a -> r2 -> CF1 r1 a
apply1R2 mf r2      = CF1 $ \ctx r1 -> unCF2 mf ctx r1 r2


--------------------------------------------------------------------------------
-- extractors 

-- | Extract the drawing context from a CtxFun.
--
-- > (ctx -> ctx)
-- 
drawingCtx      :: CF DrawingContext
drawingCtx      = CF $ \ctx -> ctx

-- | Apply the projection function to the drawing context.
--
-- > (ctx -> a) -> (ctx -> a)
--
queryCtx        :: (DrawingContext -> a) -> CF a
queryCtx f      = CF $ \ctx -> f ctx


-- | Extract the drawing context from a LocCF.
--
-- > (ctx -> pt -> ctx)
--
locCtx          :: LocCF u DrawingContext
locCtx          = CF1 $ \ctx _  -> ctx

-- | Extract the /start/ point from a LocCF.
--
-- > (ctx -> pt -> pt)
--
locPoint        :: LocCF u (Point2 u)
locPoint        = CF1 $ \_ pt -> pt


-- | Extract the drawing context from a LocThetaCF.
--
-- > (ctx -> pt -> ang -> ctx)
--
locThetaCtx     :: LocThetaCF u DrawingContext
locThetaCtx     = CF2 $ \ctx _ _ -> ctx


-- | Extract the /start/ point from a LocThetaCF.
--
-- > (ctx -> pt -> ang -> pt)
--
locThetaPoint   :: LocThetaCF u (Point2 u)
locThetaPoint   = CF2 $ \_ pt _ -> pt

-- | Extract the angle from a LocThetaCF.
--
-- > (ctx -> pt -> ang -> ang)
--
locThetaAng     :: LocThetaCF u Radian
locThetaAng     = CF2 $ \_ _ ang -> ang

-- | Extract the drawing context from a ConnectorCF.
--
-- > (ctx -> pt1 -> pt2 -> ctx)
--
connCtx         :: ConnectorCF u DrawingContext
connCtx         = CF2 $ \ctx _ _ -> ctx

-- | Extract the start point from a ConnectorCF.
--
-- > (ctx -> pt1 -> pt2 -> pt1)
--
connStart       :: ConnectorCF u (Point2 u) 
connStart       = CF2 $ \_ pt _ -> pt

-- | Extract the end point from a ConnectorCF.
--
-- > (ctx -> pt1 -> pt2 -> pt2)
--
connEnd         :: ConnectorCF u (Point2 u) 
connEnd         = CF2 $ \_ _ pt -> pt





--------------------------------------------------------------------------------
-- Combinators



infixr 1 `at`



-- | Downcast a 'LocCF' function by applying it to the supplied 
-- point, making an arity-zero Context function. 
-- 
-- Remember a 'LocCF' function is a 'CF1' context function where
-- the /static argument/ is specialized to a start point.
--
at :: LocCF u a -> Point2 u -> CF a
at = apply1R1




-- | /Chaining/ combinator - the /answer/ of the 
-- first Context function is feed to the second Context function. 
--
-- This contrasts with the usual idiom in @Wumpus-Basic@ where 
-- composite graphics are built by applying both functions to the 
-- same initial /static argument/.
--
-- Desciption:
--
-- Evaluate the first Context Function with the drawing context 
-- and the /initial state/ @st0@. The result of the evaluation is 
-- a new /state/ @st1@ and and answer @a1@. 
--
-- Evaluate the second Context Function with the drawing context 
-- and the new state @st1@, producing a new state @s2@ and an 
-- answer @a2@.
--
-- Return the result of combining the answers with 
-- @op :: (ans -> ans -> ans)@ and the second state @s2@.
--
-- @ (ctx -> s1 -> (w,s1)) -> (ctx -> s1 -> (w,s1)) -> (ctx -> s1 -> (w,s1)) @
--
-- This models chaining start points together, which is the model
-- PostScript uses for text output when successively calling the 
-- @show@ operator.
-- 
chain1 :: OPlus w 
            => CF1 s1 (s1,w) -> CF1 s1 (s1,w) -> CF1 s1 (s1,w)
chain1 f g = CF1 $ \ctx s -> let (s1,a1) = unCF1 f ctx s
                                 (s2,a2) = unCF1 g ctx s1
                             in (s2, a1 `oplus` a2)


