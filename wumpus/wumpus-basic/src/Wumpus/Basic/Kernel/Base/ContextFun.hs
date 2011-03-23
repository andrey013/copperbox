{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-}
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
  , CF3

  , Query
  , LocQuery
  , LocThetaQuery
  , ConnectorQuery


  -- * Run functions
  , runCF
  , runCF1 
  , runCF2
  , runCF3

  -- * Lift functions
  , lift0R1
  , lift0R2
  , lift1R2
  , lift0R3
  , lift1R3
  , lift2R3
 
  , promoteR1
  , promoteR2
  , promoteR3

  , apply1R1
  , apply1R2
  , apply2R2
  , apply1R3
  , apply2R3
  , apply3R3

  , uconvertR0
  , uconvertR1a
  , uconvertR2a
  , uconvertR2ab
  , uconvertR3a

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
  , rot
  , atRot
  , connect
  , chain1

  -- * Affine trafo
  , affineTransR0
  , affineTransR1a
  , affineTransR2ab
  , affineTransR2a
  , affineTransR3a

  ) where

import Wumpus.Basic.Kernel.Base.AffineTrans
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

type instance DUnit (CF (t u)) = u


-- | Variation of 'CF' with one parametric /static argument/.
--
-- The static argument is commonly a point representing the start 
-- point / origin of a drawing.
-- 
-- > CF1 :: DrawingContext -> r1 -> a 
--
newtype CF1 r1 a        = CF1 { unCF1 :: DrawingContext -> r1 -> a }

type instance DUnit (CF1 r1 (t u)) = u


-- | Variation of 'CF' with two parametric /static arguments/.
--
-- The first argument is commonly a point representing the start 
-- point / origin of a drawing. The second argument might 
-- typically be the angle of inclination (for drawing arrowheads) 
-- or an end point (for drawing connectors between two points).
-- 
-- > CF2 :: DrawingContext -> r1 -> r2 -> a 
--
newtype CF2 r1 r2 a     = CF2 { unCF2 :: DrawingContext -> r1 -> r2 -> a }

type instance DUnit (CF2 r1 r2 (t u)) = u

-- | Variation of 'CF' with three parametric /static arguments/.
--
-- > CF3 :: DrawingContext -> r1 -> r2 -> r3 -> a 
--
newtype CF3 r1 r2 r3 a = CF3 { unCF3 :: DrawingContext -> r1 -> r2 -> r3 -> a }

type instance DUnit (CF3 r1 r2 r3 (t u)) = u


-- | Alias for 'CF'. Wumpus considers Context functions that
-- don\'t produce graphics to be /queries/.
-- 
-- > Query :: DrawingContext -> a 
--
-- 'Query' has no unit type parameter.
-- 
type Query a            = CF a
 

-- | Type specialized verison of 'CF1' where the /static argument/
-- is the /start point/.
-- 
-- > LocQuery :: DrawingContext -> Point2 u -> a 
--
type LocQuery u a       = CF1 (Point2 u) a


-- | Type specialized verison of 'CF2' where the /static arguments/
-- are the /start point/ and the /angle of displacement/.
-- 
-- > LocThetaQuery :: DrawingContext -> Point2 u -> Radian -> a 
--
type LocThetaQuery u a  = CF2 (Point2 u) Radian a


-- | Type specialized verison of 'CF2' where the /static arguments/
-- are the /start point/ and the /end point/.
-- 
-- > ConnectorQuery :: DrawingContext -> Point2 u -> Point2 u -> a 
--
type ConnectorQuery u a = CF2 (Point2 u) (Point2 u) a





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

instance OPlus a => OPlus (CF3 r1 r2 r3 a)  where
  fa `oplus` fb = CF3 $ \ctx r1 r2 r3 -> 
                          unCF3 fa ctx r1 r2 r3 `oplus` unCF3 fb ctx r1 r2 r3

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

instance Monoid a => Monoid (CF3 r1 r2 r3 a) where 
  mempty          = CF3 $ \_   _  _  -> mempty
  fa `mappend` fb = CF3 $ \ctx r1 r2 r3 -> 
                      unCF3 fa ctx r1 r2 r3 `mappend` unCF3 fb ctx r1 r2 r3



-- Functor

instance Functor CF where
  fmap f ma = CF $ \ctx -> f $ unCF ma ctx 


instance Functor (CF1 r1) where
  fmap f ma = CF1 $ \ctx r1 -> f $ unCF1 ma ctx r1 

instance Functor (CF2 r1 r2) where
  fmap f ma = CF2 $ \ctx r1 r2 -> f $ unCF2 ma ctx r1 r2

instance Functor (CF3 r1 r2 r3) where
  fmap f ma = CF3 $ \ctx r1 r2 r3 -> f $ unCF3 ma ctx r1 r2 r3




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


instance Applicative (CF3 r1 r2 r3) where
  pure a    = CF3 $ \_   _  _  _  -> a
  mf <*> ma = CF3 $ \ctx r1 r2 r3 -> let f = unCF3 mf ctx r1 r2 r3
                                         a = unCF3 ma ctx r1 r2 r3
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


instance Monad (CF3 r1 r2 r3) where
  return a  = CF3 $ \_   _  _  _  -> a
  ma >>= k  = CF3 $ \ctx r1 r2 r3 -> 
                let a = unCF3 ma ctx r1 r2 r3 in (unCF3 . k) a ctx r1 r2 r3

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


instance DrawingCtxM (CF3 r1 r2 r3) where
  askDC           = CF3 $ \ctx _  _  _  -> ctx
  localize upd df = CF3 $ \ctx r1 r2 r3 -> unCF3 df (upd ctx) r1 r2 r3


--------------------------------------------------------------------------------
-- Run functions

-- | Run a /CF/ (context function) with the supplied 
-- /DrawingContext/.
--
runCF :: CF a -> DrawingContext -> a
runCF = unCF


-- | Run a /CF1/ (context function) with the supplied 
-- /DrawingContext/ and static argument.
--
runCF1 :: CF1 r1 a -> DrawingContext -> r1 -> a
runCF1 = unCF1


-- | Run a /CF1/ (context function) with the supplied 
-- /DrawingContext/ and two static arguments.
--
runCF2 :: CF2 r1 r2 a -> DrawingContext -> r1 -> r2 -> a
runCF2 = unCF2


-- | Run a /CF3/ (context function) with the supplied 
-- /DrawingContext/ and three static arguments.
--
runCF3 :: CF3 r1 r2 r3 a -> DrawingContext -> r1 -> r2 -> r3 -> a
runCF3 = unCF3


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


-- | Lift a zero-arity context function 'CF' to an arity three
-- context function 'CF3'.
-- 
lift0R3             :: CF a -> CF3 r1 r2 r3 a
lift0R3 mf          = CF3 $ \ctx _ _ _ -> unCF mf ctx


-- | Lift an arity one context function 'CF1' to an arity two
-- context function 'CF3'.
-- 
lift1R3             :: CF1 r1 a -> CF3 r1 r2 r3 a
lift1R3 mf          = CF3 $ \ctx r1 _ _ -> unCF1 mf ctx r1


-- | Lift an arity two context function 'CF2' to an arity two
-- context function 'CF3'.
-- 
lift2R3             :: CF2 r1 r2 a -> CF3 r1 r2 r3 a
lift2R3 mf          = CF3 $ \ctx r1 r2 _ -> unCF2 mf ctx r1 r2


-- Promote

-- | Promote a function @from one argument to a Context Function@ 
-- to an arity one @Context Function@.
--
-- The type signature is as explanatory as a description:
--
-- > promoteR1 :: (r1 -> CF a) -> CF1 r1 a
-- 
promoteR1           :: (r1 -> CF a) -> CF1 r1 a
promoteR1 mf        = CF1 $ \ctx r1 -> unCF (mf r1) ctx

-- | Promote a function @from two arguments to a Context Function@ 
-- to an arity two @Context Function@.
--
-- The type signature is as explanatory as a description:
--
-- > promoteR2 :: (r1 -> r2 -> CF a) -> CF2 r1 r2 a
-- 
promoteR2           :: (r1 -> r2 -> CF a) -> CF2 r1 r2 a
promoteR2 mf        = CF2 $ \ctx r1 r2 -> unCF (mf r1 r2) ctx


-- | Promote a function @from three arguments to a Context Function@ 
-- to an arity three @Context Function@.
--
-- The type signature is as explanatory as a description:
--
-- > promoteR3 :: (r1 -> r2 -> r3 -> CF a) -> CF3 r1 r2 r3 a
-- 
promoteR3           :: (r1 -> r2 -> r3 -> CF a) -> CF3 r1 r2 r3 a
promoteR3 mf        = CF3 $ \ctx r1 r2 r3 -> unCF (mf r1 r2 r3 ) ctx


-- Apply

-- | Apply an arity-one Context Function to a single argument, 
-- downcasting it by one level, making an arity-zero Context 
-- function. 
-- 
-- The type signature is as explanatory as a description:
--
-- > apply1R1 :: CF1 r1 a -> r1 -> CF a
--
apply1R1            :: CF1 r1 a -> r1 -> CF a
apply1R1 mf r1      = CF $ \ctx -> unCF1 mf ctx r1


-- | Apply an arity-two Context Function to one argument, 
-- downcasting it by one level, making an arity-one Context 
-- function. 
-- 
-- The type signature is as explanatory as a description:
--
-- > apply1R2 :: CF2 r1 r2 a -> r2 -> CF1 r1 a
--
apply1R2            :: CF2 r1 r2 a -> r2 -> CF1 r1 a
apply1R2 mf r2      = CF1 $ \ctx r1 -> unCF2 mf ctx r1 r2


-- | Apply an arity-two Context Function to two arguments, 
-- downcasting it by two levels, making an arity-zero Context 
-- function. 
-- 
-- The type signature is as explanatory as a description:
--
-- > apply2R2 :: CF2 r1 r2 a -> r1 -> r2 -> CF a
-- 
apply2R2            :: CF2 r1 r2 a -> r1 -> r2 -> CF a
apply2R2 mf r1 r2   = CF $ \ctx -> unCF2 mf ctx r1 r2


-- | Apply an arity-three Context Function to one argument, 
-- downcasting it by one level, making an arity-two Context 
-- function. 
-- 
-- The type signature is as explanatory as a description:
--
-- > apply1R3 :: CF3 r1 r2 r3 a -> r3 -> CF2 r1 r2 a
--
apply1R3            :: CF3 r1 r2 r3 a -> r3 -> CF2 r1 r2 a
apply1R3 mf r3      = CF2 $ \ctx r1 r2 -> unCF3 mf ctx r1 r2 r3


-- | Apply an arity-three Context Function to two arguments, 
-- downcasting it by two levels, making an arity-one Context 
-- function. 
-- 
-- The type signature is as explanatory as a description:
--
-- > apply2R3 :: CF3 r1 r2 a -> r2 -> r3 -> CF1 r1 a
--
apply2R3            :: CF3 r1 r2 r3 a -> r2 -> r3 -> CF1 r1 a
apply2R3 mf r2 r3   = CF1 $ \ctx r1 -> unCF3 mf ctx r1 r2 r3


-- | Apply an arity-three Context Function to three arguments, 
-- downcasting it by three levels, making an arity-zero Context 
-- function. 
-- 
-- The type signature is as explanatory as a description:
--
-- > apply3R3 :: CF3 r1 r2 r3 a -> r1 -> r2 -> r3 -> CF a
-- 
apply3R3              :: CF3 r1 r2 r3 a -> r1 -> r2 -> r3 -> CF a
apply3R3 mf r1 r2 r3  = CF $ \ctx -> unCF3 mf ctx r1 r2 r3



--------------------------------------------------------------------------------

-- DESIGN NOTE
--
-- This is one place where abstracting over arity would be 
-- especially useful. Unfortuantely it is not possible - even 
-- though the type synonyms LocImage, PosImage etc. make the 
-- actual graphic objects tantalizing uniform. The problem
-- is the synonyms have the similarity not the actaul types.
-- 
-- The letter suffixes indicate which inputr arguments are 
-- transformed.
--


-- | This converts Image.
--
uconvertR0 :: (Functor t, InterpretUnit u, InterpretUnit u1) 
           => CF (t u) -> CF (t u1)
uconvertR0 df = CF $ \ctx -> uconvertF (dc_font_size ctx) $ unCF df ctx


-- | This converts LocImage.
--
uconvertR1a :: (InterpretUnit u, InterpretUnit u1, Functor t)
            => CF1 (Point2 u)  (t u) 
            -> CF1 (Point2 u1) (t u1)
uconvertR1a df = CF1 $ \ctx pt -> 
    let ptu = uconvertF (dc_font_size ctx) pt 
    in uconvertF (dc_font_size ctx) $ unCF1 df ctx ptu

-- | This converts LocThetaImage and PosImage.
--
uconvertR2a :: (InterpretUnit u, InterpretUnit u1, Functor t)
            => CF2 (Point2 u)  r2 (t u) 
            -> CF2 (Point2 u1) r2 (t u1)
uconvertR2a df = CF2 $ \ctx pt r2 -> 
    let ptu = uconvertF (dc_font_size ctx) pt 
    in uconvertF (dc_font_size ctx) $ unCF2 df ctx ptu r2


-- | This converts ConnectorImage.
--
uconvertR2ab :: (InterpretUnit u, InterpretUnit u1, Functor t)
            => CF2 (Point2 u)  (Point2 u)  (t u) 
            -> CF2 (Point2 u1) (Point2 u1) (t u1)
uconvertR2ab df = CF2 $ \ctx p0 p1 -> 
    let p0u = uconvertF (dc_font_size ctx) p0
        p1u = uconvertF (dc_font_size ctx) p1
    in uconvertF (dc_font_size ctx) $ unCF2 df ctx p0u p1u


-- | This converts PosThetaImage.
--
uconvertR3a :: (InterpretUnit u, InterpretUnit u1, Functor t)
            => CF3 (Point2 u)  r2 r3 (t u) 
            -> CF3 (Point2 u1) r2 r3 (t u1)
uconvertR3a df = CF3 $ \ctx pt r2 r3 -> 
    let ptu = uconvertF (dc_font_size ctx) pt 
    in uconvertF (dc_font_size ctx) $ unCF3 df ctx ptu r2 r3


--------------------------------------------------------------------------------
-- extractors 

-- | Extract the drawing context from a CtxFun.
--
-- > (ctx -> ctx)
-- 
drawingCtx      :: Query DrawingContext
drawingCtx      = CF $ \ctx -> ctx

-- | Apply the projection function to the drawing context.
--
-- > (ctx -> a) -> (ctx -> a)
--
queryCtx        :: (DrawingContext -> a) -> Query a
queryCtx f      = CF $ \ctx -> f ctx


-- | Extract the drawing context from a LocQuery.
--
-- > (ctx -> pt -> ctx)
--
locCtx          :: LocQuery u DrawingContext
locCtx          = CF1 $ \ctx _  -> ctx

-- | Extract the /start/ point from a LocCF.
--
-- > (ctx -> pt -> pt)
--
locPoint        :: LocQuery u (Point2 u)
locPoint        = CF1 $ \_ pt -> pt


-- | Extract the drawing context from a LocThetaCF.
--
-- > (ctx -> pt -> ang -> ctx)
--
locThetaCtx     :: LocThetaQuery u DrawingContext
locThetaCtx     = CF2 $ \ctx _ _ -> ctx


-- | Extract the /start/ point from a LocThetaCF.
--
-- > (ctx -> pt -> ang -> pt)
--
locThetaPoint   :: LocThetaQuery u (Point2 u)
locThetaPoint   = CF2 $ \_ pt _ -> pt

-- | Extract the angle from a LocThetaCF.
--
-- > (ctx -> pt -> ang -> ang)
--
locThetaAng     :: LocThetaQuery u Radian
locThetaAng     = CF2 $ \_ _ ang -> ang

-- | Extract the drawing context from a ConnectorCF.
--
-- > (ctx -> pt1 -> pt2 -> ctx)
--
connCtx         :: ConnectorQuery u DrawingContext
connCtx         = CF2 $ \ctx _ _ -> ctx

-- | Extract the start point from a ConnectorCF.
--
-- > (ctx -> pt1 -> pt2 -> pt1)
--
connStart       :: ConnectorQuery u (Point2 u) 
connStart       = CF2 $ \_ pt _ -> pt

-- | Extract the end point from a ConnectorCF.
--
-- > (ctx -> pt1 -> pt2 -> pt2)
--
connEnd         :: ConnectorQuery u (Point2 u) 
connEnd         = CF2 $ \_ _ pt -> pt





--------------------------------------------------------------------------------
-- Combinators



infixr 1 `at`


-- | Downcast a 'LocCF' function by applying it to the supplied 
-- point, making an arity-zero Context Function. 
-- 
-- Remember a 'LocCF' function is a 'CF1' context function where
-- the /static argument/ is specialized to a start point.
--
at :: LocQuery u a -> Point2 u -> CF a
at = apply1R1


-- TODO - @rot@ show really be called something to indicate 
-- @inclination@ rather than /rotation/.

infixr 1 `rot`


-- | Downcast a 'LocThetaQuery' function by applying it to the 
-- supplied angle, making an arity-one Context Function (a 
-- 'LocCF'). 
-- 

rot :: LocThetaQuery u a -> Radian -> LocQuery u a
rot = apply1R2


-- | Downcast a 'LocThetaQuery' function by applying it to the 
-- supplied point and angle, making an arity-zero Context 
-- Function (a 'CF'). 
--
atRot :: LocThetaQuery u a -> Point2 u -> Radian -> CF a
atRot = apply2R2


-- | Downcast a 'ConnectorQuery' function by applying it to the 
-- start and end point, making an arity-zero Context Function 
-- (a 'CF'). 
-- 
connect :: ConnectorQuery u a -> Point2 u -> Point2 u -> CF a
connect = apply2R2



infixr 6 `chain1`

-- | /Chaining/ combinator - the /answer/ of the 
-- first Context Function is feed to the second Context Function. 
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


--------------------------------------------------------------------------------
-- Affine trafos

--
-- DESIGN NOTE
--
-- Unfortunately the affine classes really don\'t work well with 
-- contextual units. We have to metamorph in an out of Doubles to 
-- use them.
--

affineTransR0 :: (InterpretUnit u) 
              => (FontSize -> t u -> t u)
              -> CF (t u)
              -> CF (t u)
affineTransR0 fn df = CF $ \ctx -> 
    let sz    = dc_font_size ctx in fn sz $ unCF df ctx


-- Arity 1

affineTransR1a :: (InterpretUnit u) 
               => (FontSize -> Point2 u -> Point2 u) 
               -> (FontSize -> t u -> t u) 
               -> CF1 (Point2 u) (t u)
               -> CF1 (Point2 u) (t u)
affineTransR1a f g df = CF1 $ \ctx pt -> 
    let sz    = dc_font_size ctx
    in g sz $ unCF1 df ctx (f sz pt)


-- Connectors


affineTransR2ab :: InterpretUnit u 
               => (FontSize -> Point2 u -> Point2 u) 
               -> (FontSize -> t u -> t u) 
               -> CF2 (Point2 u) (Point2 u) (t u)
               -> CF2 (Point2 u) (Point2 u) (t u)
affineTransR2ab f g df = CF2 $ \ctx p0 p1 -> 
    let sz    = dc_font_size ctx
    in g sz $ unCF2 df ctx (f sz p0) (f sz p1)




-- Unfortunately, we cannot really have instances for polymorphic 
-- arguments at position 2, as they would conflict with the 
-- connector instance:
--
-- > -- Not allowed...
-- >
-- > instance (Translate a, InterpretUnit u) => 
-- >    Translate (CF2 (Point2 u) r2 a) where
--
-- Instead we have to provide the means to roll-your-own.
--

-- | Roll-you-own helper for affine instances of CF2.
--
affineTransR2a :: InterpretUnit u
               => (FontSize -> Point2 u -> Point2 u) 
               -> (FontSize -> t u -> t u) 
               -> CF2 (Point2 u) r2 (t u)
               -> CF2 (Point2 u) r2 (t u)
affineTransR2a f g df = CF2 $ \ctx pt r2 -> 
    let sz    = dc_font_size ctx
    in g sz $ unCF2 df ctx (f sz pt) r2


-- | Roll-you-own helper for affine instances of CF3.
--
affineTransR3a :: InterpretUnit u
               => (FontSize -> Point2 u -> Point2 u) 
               -> (FontSize -> t u -> t u) 
               -> CF3 (Point2 u) r2 r3 (t u)
               -> CF3 (Point2 u) r2 r3 (t u)
affineTransR3a f g df = CF3 $ \ctx pt r2 r3 -> 
    let sz    = dc_font_size ctx
    in g sz $ unCF3 df ctx (f sz pt) r2 r3


-- These CF instance cover Images and Graphics.

instance (CtxRotate t u, InterpretUnit u) => 
    Rotate (CF (t u)) where
  rotate ang = affineTransR0 (\sz -> ctxRotate sz ang)

instance (CtxRotateAbout t u, InterpretUnit u, u ~ DUnit (t u)) => 
    RotateAbout (CF (t u)) u where
  rotateAbout ang p0 = affineTransR0 (\sz -> ctxRotateAbout sz ang p0)

instance (CtxScale t u, InterpretUnit u) => 
    Scale (CF (t u)) where
  scale sx sy = affineTransR0 (\sz -> ctxScale sz sx sy)

instance (CtxTranslate t u, InterpretUnit u, u ~ DUnit (t u)) => 
    Translate (CF (t u)) u where
  translate dx dy = affineTransR0 (\sz -> ctxTranslate sz dx dy)


-- These CF1 instances cover LocGraphic and LocImage.

instance (CtxRotate t u, Functor t, InterpretUnit u) => 
    Rotate (CF1 (Point2 u) (t u)) where
  rotate ang = affineTransR1a (\sz -> ctxRotate sz ang) 
                              (\sz -> ctxRotate sz ang)

instance (CtxRotateAbout t u, InterpretUnit u, u ~ DUnit (t u)) => 
    RotateAbout (CF1 (Point2 u) (t u)) u where
  rotateAbout ang p0 = affineTransR1a (\sz -> ctxRotateAbout sz ang p0)
                                      (\sz -> ctxRotateAbout sz ang p0)
     
instance (CtxScale t u, InterpretUnit u) => 
    Scale (CF1 (Point2 u) (t u)) where
  scale sx sy = affineTransR1a (\sz -> ctxScale sz sx sy)
                               (\sz -> ctxScale sz sx sy)

instance (CtxTranslate t u, InterpretUnit u, u ~ DUnit (t u)) => 
    Translate (CF1 (Point2 u) (t u)) u where
  translate dx dy = affineTransR1a (\sz -> ctxTranslate sz dx dy) 
                                   (\sz -> ctxTranslate sz dx dy)


-- These CF2 instances cover ConnectorGraphic and ConnectorImage.

instance (CtxRotate t u, Functor t, InterpretUnit u) => 
    Rotate (CF2 (Point2 u) (Point2 u) (t u)) where
  rotate ang = affineTransR2ab (\sz -> ctxRotate sz ang) 
                               (\sz -> ctxRotate sz ang)


instance (CtxRotateAbout t u, InterpretUnit u, u ~ DUnit (t u)) => 
    RotateAbout (CF2 (Point2 u) (Point2 u) (t u)) u where
  rotateAbout ang p0 = affineTransR2ab (\sz -> ctxRotateAbout sz ang p0)
                                       (\sz -> ctxRotateAbout sz ang p0)

instance (CtxScale t u, InterpretUnit u) => 
    Scale (CF2 (Point2 u) (Point2 u) (t u)) where
  scale sx sy = affineTransR2ab (\sz -> ctxScale sz sx sy)
                                (\sz -> ctxScale sz sx sy)

instance (CtxTranslate t u, InterpretUnit u, u ~ DUnit (t u)) => 
    Translate (CF2 (Point2 u) (Point2 u) (t u)) u where
  translate dx dy = affineTransR2ab (\sz -> ctxTranslate sz dx dy) 
                                    (\sz -> ctxTranslate sz dx dy)




-- These CF2 instances cover LocThetaGraphic and LocThetaImage.

instance (CtxRotate t u, Functor t, InterpretUnit u) => 
    Rotate (CF2 (Point2 u) Radian (t u)) where
  rotate ang = affineTransR2a (\sz -> ctxRotate sz ang) 
                              (\sz -> ctxRotate sz ang)

instance (CtxRotateAbout t u, InterpretUnit u, u ~ DUnit (t u)) => 
    RotateAbout (CF2 (Point2 u) Radian (t u)) u where
  rotateAbout ang p0 = affineTransR2a (\sz -> ctxRotateAbout sz ang p0)
                                      (\sz -> ctxRotateAbout sz ang p0)

instance (CtxScale t u, InterpretUnit u) => 
    Scale (CF2 (Point2 u) Radian (t u)) where
  scale sx sy = affineTransR2a (\sz -> ctxScale sz sx sy)
                               (\sz -> ctxScale sz sx sy)

instance (CtxTranslate t u, InterpretUnit u, u ~ DUnit (t u)) => 
    Translate (CF2 (Point2 u) Radian (t u)) u where
  translate dx dy = affineTransR2a (\sz -> ctxTranslate sz dx dy) 
                                   (\sz -> ctxTranslate sz dx dy)


