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

  -- * Note - CF types only temporarily exposed...
    CF (..)    
  , CF1 (..)
  , CF2 (..)

  , LocCF
  , LocThetaCF
  , ConnectorCF
  , DLocCF
  , DLocThetaCF
  , DConnectorCF



  -- * Run functions
  , runCF

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

  -- * Reducers
  , unLoc
  , unTheta
  , unLocTheta
  , unConnector

  , getCF1
  , getCF2

  -- * Combinators
  , at

--   , localPoint

  , wrap
  , wrap1
  , wrap2

  , promote1
  , promote2

  , static1 
  , static2
  , dblstatic

  , bind
  , bind1
  , bind2


  , situ1
  , situ2

  , apply
  , apply1
  , apply2

  -- * Pre-transformers
  , prepro1
  , prepro2
  , prepro2a
  , prepro2b

  -- * Post-transformers
  , postpro
  , postpro1
  , postpro2

  -- * Post-combiners
  , postcomb
  , postcomb1
  , postcomb2

  , accumulate1


  ) where

import Wumpus.Basic.Kernel.Base.BaseDefs
import Wumpus.Basic.Kernel.Base.DrawingContext

import Wumpus.Core                              -- package: wumpus-core


import Control.Applicative
import Data.Monoid


--------------------------------------------------------------------------------
--

-- | Most drawing operations in Wumpus-Basic have an implicit 
-- /graphics state/ the @DrawingContext@, so the most primitive 
-- building block is a function from the DrawingContext to some 
-- polymorphic answer.
-- 
-- This functional type is represented concretely as the initials 
-- @CF@ for /contextual function/.
-- 
-- > CF :: DrawingContext -> a 
--
newtype CF a            = CF  { unCF :: DrawingContext -> a }



newtype CF1 r1 a        = CF1 { unCF1 :: DrawingContext -> r1 -> a }

newtype CF2 r1 r2 a     = CF2 { unCF2 :: DrawingContext -> r1 -> r2 -> a }

 

type LocCF          u a = CF1 (Point2 u) a

type LocThetaCF     u a = CF2 (Point2 u) Radian a

type ConnectorCF    u a = CF2 (Point2 u) (Point2 u) a



type DLocCF a           = LocCF       Double a
type DLocThetaCF a      = LocThetaCF  Double a
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


--------------------------------------------------------------------------------
-- Run functions



-- Code below is now old ...








-- | Run a /CF/ (context function) with the supplied /DrawingContext/.
--
runCF :: DrawingContext -> CF a -> a
runCF ctx df = unCF df ctx


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
-- /Reducers/

-- | This is unCF1 at a specific type.
--
unLoc :: Point2 u -> LocCF u a -> CF a
unLoc pt mf = CF $ \ctx -> unCF1 mf ctx pt

unTheta :: Radian -> LocThetaCF u a -> LocCF u a
unTheta theta mf = CF1 $ \ctx pt -> unCF2 mf ctx pt theta

unLocTheta :: Point2 u -> Radian -> LocThetaCF u a -> CF a
unLocTheta pt theta mf = CF $ \ctx -> unCF2 mf ctx pt theta

unConnector :: Point2 u -> Point2 u -> ConnectorCF u a -> CF a
unConnector p0 p1 mf = CF $ \ctx -> unCF2 mf ctx p0 p1


getCF1 :: r1 -> CF (r1 -> a) -> CF a
getCF1 a mf = CF $ \ctx -> unCF mf ctx a

getCF2 :: r1 -> r2 ->  CF (r1 -> r2 -> a) -> CF a
getCF2 a b mf = CF $ \ctx -> unCF mf ctx a b




--------------------------------------------------------------------------------
-- Combinators



infixr 1 `at`
at :: LocCF u b -> Point2 u -> CF b
at = flip unLoc

-- localPoint :: (Point2 u -> Point2 u) -> LocCF u a -> LocCF u a
-- localPoint = prepro1


-- What type of composition is this?

comp1 :: CF (r1 -> a) -> CF (a -> b) -> CF (r1 -> b)
comp1 mf mg = CF $ \ctx r1 -> unCF mg ctx $ unCF mf ctx r1

-- Is comp2 the two arity version?

comp2 :: CF (r1 -> r2 -> a) -> CF (a -> b) -> CF (r1 -> r2 -> b)
comp2 mf mg = CF $ \ctx r1 r2 -> unCF mg ctx $ unCF mf ctx r1 r2


-- | Lift a pure value into a Context functional. The 
-- DrawingContext is ignored.
--
-- > ans -> (ctx -> ans)
--
-- This is Applicative\'s @pure@. However, it extends to an 
-- /arity family/ of @wrap@ combinators.
--
wrap :: a -> CF a
wrap = pure


-- | Lift a pure value into a Context functional, ignoring both 
-- the DrawingContext and the /static/ argument, e.g. this would
-- ingnore start point for a @LocDrawing@).
--
-- > ans -> (ctx -> r1 -> ans)
--
wrap1 :: a -> CF (r1 -> a)
wrap1 = pure . pure

-- | Lift a pure value into a Context functional, ignoring both 
-- the DrawingContext and the two /static/ arguments, e.g. this 
-- would ignore the start point and angle for a @LocThetaDrawing@.
--
-- > ans -> (ctx -> r1 -> r2 -> ans)
--
wrap2 :: a -> CF (r1 -> r2 -> a)
wrap2 = pure . pure . pure

   

-- | Promote a Context functional with one argument /outside/
-- the functional so that the the argument is /inside/ the 
-- Context functional.
--
-- The type signature is probably more illustrative of the 
-- operation than this description:
--
-- > (r1 -> ctx -> ans) -> (ctx -> r1 -> ans)
--
-- This is essentially the @cardinal@ combinator - @flip@
-- in Haskell.
--
promote1 :: (r1 -> CF ans) -> CF (r1 -> ans)
promote1 f = CF $ \ctx a -> unCF (f a) ctx


-- | Promote a Context functional with two arguments /outside/
-- the functional so that the two arguments are /inside/ the 
-- Context functional.
--
-- The type signature is probably more illustrative of the 
-- operation than this description:
--
-- > (r1 -> r2 -> ctx -> ans) -> (ctx -> r1 -> r2 -> ans)
--
promote2 :: (r1 -> r2 -> CF ans) -> CF (r1 -> r2 -> ans)
promote2 df = CF $ \ctx a b -> unCF (df a b) ctx


-- | Extend the arity of a /Context functional/, the original 
-- function is oblivious to the added argument.
--
-- Typically this combinator is used to take a @Graphic@ to a
-- @LocGraphic@ ingoring the start point (figuratively a @Graphic@ 
-- is not /coordinate free/). 
--
-- > (ctx -> ans) -> (ctx -> r1 -> ans)
--
-- This was called the J-combinator by Joy, Rayward-Smith and
-- Burton (ref. /Compling Functional Languages/ by Antoni 
-- Diller), however it is not the J combinator commonly in the 
-- Literature. 
--
static1 :: CF ans -> CF (r1 -> ans)
static1 df = CF $ \ctx _ -> unCF df ctx


-- | Extend the arity of a /Context functional/, the original 
-- function is oblivious to the added argument.
--
-- Typically this combinator is used to take a @LocGraphic@ to a
-- @LocThetaGraphic@ ingoring the angle of direction.
--
-- > (ctx -> r1 -> ans) -> (ctx -> r1 -> r2 -> ans)
--
-- This was called the J-Prime combinator by Joy, Rayward-Smith 
-- and Burton (ref. /Compling Functional Languages/ by Antoni 
-- Diller). 
--
static2 :: CF (r1 -> ans) -> CF (r1 -> r2 -> ans)
static2 df = CF $ \ctx a _ -> unCF df ctx a


-- | Complementary combinator to static2. 
--
-- This combinator raises a function two levels rather than one.
--
-- > (ctx -> ans) -> (ctx -> r1 -> r2 -> ans)
--
dblstatic :: CF ans -> CF (r1 -> r2 -> ans)
dblstatic df = CF $ \ctx _ _ -> unCF df ctx


infixl 1 `bind`, `bind1`, `bind2`


-- | Supply the output from the first function to the second 
-- function.
--
-- This is just monadic bind - specialized to the CF functional 
-- type.
--
-- > (ctx -> a) -> (a -> ctx -> ans) -> (ctx -> ans)
-- 
bind :: CF a -> (a -> CF ans) -> CF ans
bind df dk = CF $ \ctx -> let z = unCF df ctx in unCF (dk z) ctx


-- | Supply the output from the first function to the second 
-- function, /sharing/ the drawing context and the static 
-- argument @r1@.
--
-- > (ctx -> r1 -> a) -> (a -> ctx -> -> r1 -> ans) -> (ctx -> r1 -> ans)
-- 
bind1 :: CF (r1 -> a) -> (a -> CF (r1 -> ans)) -> CF (r1 -> ans)
bind1 df dk = CF $ \ctx a -> let z = unCF df ctx a in unCF (dk z) ctx a

-- | Supply the output from the first function to the second 
-- function, /sharing/ the DrawingContext and the two static 
-- arguments @r1@ and @r2@.
--
-- > (ctx -> r1 -> r2 -> a) -> (a -> ctx -> -> r1 -> r2 -> ans) -> (ctx -> r1 -> r2 -> ans)
-- 
bind2 :: CF (r1 -> r2 -> a) -> (a -> CF (r1 -> r2 -> ans)) 
      -> CF (r1 -> r2 -> ans)
bind2 df dk = CF $ \ctx a b -> 
    let z = unCF df ctx a b in unCF (dk z) ctx a b


-- idstar  :: (r1 -> r2 -> ans) -> r1 -> r2 -> ans

-- | Supply the arguments to an arity 1 Context functional so it 
-- can be /situated/. Typically this is supplying the start point 
-- to a @LocGraphic@ or @LocImage@.
--
-- > (ctx -> r1 -> ans) -> r1 -> (ctx -> ans)
--
-- This is equivalent to the @id**@ combinator.
--
situ1 :: CF (r1 -> ans) -> r1 -> CF ans
situ1 df a = CF $ \ctx -> unCF df ctx a



-- | Supply the arguments to an arity 2 Conterxt functional so 
-- it can be /situated/. Typically this is supplying the start 
-- point and angle to a @LocThetaGraphic@ or @LocThetaImage@.
--
-- > (ctx -> r1 -> r2 -> ans) -> r1 -> r2 -> (ctx -> ans)
--
situ2 :: CF (r1 -> r2 -> ans) -> r1 -> r2 -> CF ans
situ2 df a b = CF $ \ctx -> unCF df ctx a b


-- | Apply the the functional produced by the first argument to
-- the value produced by the second.
--
-- > (ctx -> a -> ans) -> (ctx -> a) -> (ctx -> ans) 
--
apply :: CF (a -> ans) -> CF a -> CF ans
apply df da = CF $ \ctx -> unCF df ctx (unCF da ctx)


-- | Apply the the functional produced by the first argument to
-- the value produced by the second /sharing/ the context of the 
-- first functional argument @r1@ (usually a Point2) as well as 
-- the DrawingContext.
--
-- > (ctx -> r1 -> a -> ans) -> (ctx -> r1 -> a) -> (ctx -> r1 -> ans) 
--
apply1 :: CF (r1 -> a -> ans) -> CF (r1 -> a) -> CF (r1 -> ans)
apply1 df da = CF $ \ctx a -> unCF df ctx a (unCF da ctx a)


-- | Apply the the functional produced by the first argument to
-- the value produced by the second /sharing/ the context of the 
-- two functional arguments @r1@ and @r2@ as well as the 
-- DrawingContext.
--
-- > (ctx -> r1 -> r2 -> a -> ans) -> (ctx -> r1 -> r2 -> a) -> (ctx -> r1 -> r2 -> ans) 
--
apply2 :: CF (r1 -> r2 -> a -> ans) -> CF (r1 -> r2 -> a) 
       -> CF (r1 -> r2 -> ans)
apply2 df da = CF $ \ctx a b -> unCF df ctx a b (unCF da ctx a b)


--------------------------------------------------------------------------------
-- Pre-transformers

-- The primary /pre-transformation/ function is @localize@, here 
-- it is without the newtype wrapper or the specific types:
--
-- > fn :: (ctx -> ctx) -> (ctx -> ans) -> ctx -> ans
-- > fn f g = \ctx -> g (f ctx)
--
-- This a a type restricted version of reverse appliction, the T
-- combinator sometimes called queer, sometings @(#)@ in Haskell.
-- In Wumpus-Basic it is @localize@ which is overloaded for the
-- CF newtype and TraceDrawing monads.
--
-- > localizeDR :: (DrawingContext -> DrawingContext) -> CF a -> CF a
-- > localizeDR = localize
--
-- Figuartively speaking this is an opposite of the fmap function 
-- instance. Instead of (post-) transforming the output, ... 
-- (pre-) transforms  the input.
-- 
-- Due to Context functionals being stacked in a particular way
-- @localize@ always pre-transforms the DrawingContext regardless
-- of the arity of the functional:
--
-- > localizeDR2 :: (DrawingContext -> DrawingContext) 
-- >             -> CF (r -> a) -> CF (r -> a)
-- > localizeDR2 = localize
-- 


-- | Apply the static argument transfomer @(r1 -> a)@ to the 
-- static argument /before/ applying the Context functional.
--
-- > (r1 -> a) -> (ctx -> a -> ans) -> (ctx -> r1 -> ans)
-- 
prepro1 :: (r1 -> a) -> CF (a -> ans) -> CF (r1 -> ans)
prepro1 f mf = (pure $ \ a -> f a) `comp1` mf



-- | Apply the static argument transfomers to their respective
-- static arguments /before/ applying the Context functional.
--
-- > (r1 -> a) -> (r2 -> b) -> (ctx -> a -> b -> ans) -> (ctx -> r1 -> r2 -> ans)
-- 
prepro2 :: (r1 -> a) -> (r2 -> b) -> CF (a -> b -> ans) -> CF (r1 -> r2 -> ans)
prepro2 f g mf = CF $ \ctx a b -> unCF mf ctx (f a) (g b)


-- | Apply the static argument transfomer to the first static
-- argument of a two static argument functional /before/ applying 
-- the Context functional.
--
-- > (r1 -> a) -> (ctx -> a -> r2 -> ans) -> (ctx -> r1 -> r2 -> ans)
-- 
prepro2a :: (r1 -> a) -> CF (a -> r2 -> ans) -> CF (r1 -> r2 -> ans)
prepro2a f mf = CF $ \ctx a b -> unCF mf ctx (f a) b

-- | Apply the static argument transfomer to the second static
-- argument of a two static argument functional /before/ applying 
-- the Context functional.
--
-- > (r2 -> a) -> (ctx -> r1 -> a -> ans) -> (ctx -> r1 -> r2 -> ans)
-- 
prepro2b :: (r2 -> a) -> CF (r1 -> a -> ans) -> CF (r1 -> r2 -> ans)
prepro2b f mf = CF $ \ctx a b -> unCF mf ctx a (f b)

------------------------------------------------------------------------------
-- Post-transfomers

-- | Apply the post-transformer to the result of the Context
-- functional.
--
-- > (a -> ans) -> (ctx -> a) -> (ctx -> ans) 
--
postpro :: (a -> ans) -> CF a -> CF ans
postpro = fmap

-- | Apply the post-transformer to the result of the Context
-- functional. Version for one static argument.
--
-- Note - the DrawingContext is always present so it is never 
-- counted as a static argument.
--
-- > (a -> ans) -> (ctx -> r1 -> a) -> (ctx -> r1 -> ans) 
--
postpro1 :: (a -> ans) -> CF (r1 -> a) -> CF (r1 -> ans)
postpro1 = postpro . fmap  

-- | Apply the post-transformer to the result of the Context
-- functional. Version for two static arguments.
--
-- Note - the DrawingContext is always present so it is never 
-- counted as a static argument.
--
--
-- > (a -> ans) -> (ctx -> r1 -> r2 -> a) -> (ctx -> r1 -> r2 -> ans) 
--
postpro2 :: (a -> ans) -> CF (r1 -> r2 -> a) -> CF (r1 -> r2 -> ans)
postpro2 = postpro . fmap . fmap


--------------------------------------------------------------------------------
-- Post-combiners

-- | Combine the results of the two Context Functions with the 
-- supplied operator.
--
-- > (a -> b -> ans) -> (ctx -> a) -> (ctx -> b) -> (ctx -> ans)
--
postcomb :: (a -> b -> ans) -> CF a -> CF b -> CF ans
postcomb op df dg = CF $ \ctx ->  unCF df ctx `op` unCF dg ctx

-- | Combine the results of the two one-static-argument Context 
-- Functions with the supplied operator.
--
-- > (a -> b -> ans) -> (ctx -> r1 -> a) -> (ctx -> r1 -> b) -> (ctx -> r1 -> ans)
--
postcomb1 :: (a -> b -> c) -> CF (r1 -> a) -> CF (r1 -> b) -> CF (r1 -> c)
postcomb1 op df dg = CF $ \ctx a -> unCF df ctx a `op` unCF dg ctx a


-- | Combine the results of the two two-static-argument Context 
-- Functions with the supplied operator.
--
-- > (a -> b -> ans) -> (ctx -> r1 -> a) -> (ctx -> r1 -> b) -> (ctx -> r1 -> ans)
--
postcomb2 :: (a -> b -> ans) -> CF (r1 -> r2 -> a) -> CF (r1 -> r2 -> b) 
          -> CF (r1 -> r2 -> ans)
postcomb2 op df dg = CF $ \ctx a b -> 
    unCF df ctx a b `op` unCF dg ctx a b



-- | Iteration combinator - the initial argument @s1@ is not 
-- shared bewteen the drawings.
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
-- PostScript uses for text output when succesively calling the 
-- @show@ operator.
-- 
accumulate1 :: OPlus w 
            => CF1 s1 (s1,w) -> CF1 s1 (s1,w) -> CF1 s1 (s1,w)
accumulate1 f g = CF1 $ \ctx s -> 
                         let (s1,a1) = unCF1 f ctx s
                             (s2,a2) = unCF1 g ctx s1
                         in (s2, a1 `oplus` a2)


{-

-- | Version of accumulate1 - this is a synthetic 
-- combinator and is not expected to be useful!
--
-- @ (ctx -> s1 -> r2 -> (w,s1)) @
-- @     -> (ctx -> s1 -> r2 -> (w,s1)) @
-- @     -> (ctx -> s1 -> r2 -> (w,s1)) @
--
accumulate2 :: OPlus w 
            => CF (s1 -> r2 -> (s1,w)) 
            -> CF (s1 -> r2 -> (s1,w))
            -> CF (s1 -> r2 -> (s1,w))
accumulate2 f g = CF $ \ctx s t -> 
                         let (s1,a1) = unCF f ctx s t
                             (s2,a2) = unCF g ctx s1 t
                         in (s2, a1 `oplus` a2)

-}


