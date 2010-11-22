{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Graphic.ContextFunction
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  highly unstable
-- Portability :  GHC 
--
-- The primary drawing type and base combinators to manipulate it. 
--
--------------------------------------------------------------------------------

module Wumpus.Basic.Graphic.ContextFunction
  (

    CF

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

  , unCF1
  , unCF2

  -- * Combinators
  , wrap
  , wrap1
  , wrap2

  , promote1
  , promote2

  , raise
  , raise1
  , raise2

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
  , accumulate2


  ) where

-- import Wumpus.Basic.Graphic.Anchors
import Wumpus.Basic.Graphic.Base
import Wumpus.Basic.Graphic.DrawingContext

import Wumpus.Core                      -- package: wumpus-core


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
newtype CF a = CF { getCF :: DrawingContext -> a }


type LocCF          u a = CF (Point2 u -> a)

type LocThetaCF     u a = LocCF u (Radian -> a)

type ConnectorCF    u a = LocCF u (Point2 u -> a)



type DLocCF a           = LocCF       Double a
type DLocThetaCF a      = LocThetaCF  Double a
type DConnectorCF a     = ConnectorCF Double a



--------------------------------------------------------------------------------
-- Drawing instances

instance Functor CF where
  fmap f ma = CF $ \ctx -> f $ getCF ma ctx 


instance OPlus a => OPlus (CF a)  where
  fa `oplus` fb = CF $ \ctx -> getCF fa ctx `oplus` getCF fb ctx

-- The monoid instance seems sensible...
--
instance Monoid a => Monoid (CF a) where 
  mempty          = CF $ \_   -> mempty
  fa `mappend` fb = CF $ \ctx -> getCF fa ctx `mappend` getCF fb ctx

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


instance DrawingCtxM CF where
  askDC           = CF $ \ctx -> ctx
  localize upd df = CF $ \ctx -> getCF df (upd ctx)
  



--------------------------------------------------------------------------------
-- Run functions



-- | Run a /CF/ (context function) with the supplied /DrawingContext/.
--
runCF :: DrawingContext -> CF a -> a
runCF ctx df = getCF df ctx


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
locCtx          = CF $ \ctx _  -> ctx

-- | Extract the /start/ point from a LocCF.
--
-- > (ctx -> pt -> pt)
--
locPoint        :: LocCF u (Point2 u)
locPoint        = CF $ \_ pt -> pt


-- | Extract the drawing context from a LocThetaCF.
--
-- > (ctx -> pt -> ang -> ctx)
--
locThetaCtx     :: LocThetaCF u DrawingContext
locThetaCtx     = CF $ \ctx _ _ -> ctx


-- | Extract the /start/ point from a LocThetaCF.
--
-- > (ctx -> pt -> ang -> pt)
--
locThetaPoint   :: LocThetaCF u (Point2 u)
locThetaPoint   = CF $ \_ pt _ -> pt

-- | Extract the angle from a LocThetaCF.
--
-- > (ctx -> pt -> ang -> ang)
--
locThetaAng     :: LocThetaCF u Radian
locThetaAng     = CF $ \_ _ ang -> ang

-- | Extract the drawing context from a ConnectorCF.
--
-- > (ctx -> pt1 -> pt2 -> ctx)
--
connCtx         :: ConnectorCF u DrawingContext
connCtx         = CF $ \ctx _ _ -> ctx

-- | Extract the start point from a ConnectorCF.
--
-- > (ctx -> pt1 -> pt2 -> pt1)
--
connStart       :: ConnectorCF u (Point2 u) 
connStart       = CF $ \_ pt _ -> pt

-- | Extract the end point from a ConnectorCF.
--
-- > (ctx -> pt1 -> pt2 -> pt2)
--
connEnd         :: ConnectorCF u (Point2 u) 
connEnd         = CF $ \_ _ pt -> pt


--------------------------------------------------------------------------------
-- /Reducers/

-- | This is unCF1 at a specific type.
--
unLoc :: Point2 u -> LocCF u a -> CF a
unLoc pt mf = CF $ \ctx -> getCF mf ctx pt

unTheta :: Radian -> LocThetaCF u a -> LocCF u a
unTheta theta mf = CF $ \ctx pt -> getCF mf ctx pt theta

unLocTheta :: Point2 u -> Radian -> LocThetaCF u a -> CF a
unLocTheta pt theta mf = CF $ \ctx -> getCF mf ctx pt theta

unConnector :: Point2 u -> Point2 u -> ConnectorCF u a -> CF a
unConnector p0 p1 mf = CF $ \ctx -> getCF mf ctx p0 p1


unCF1 :: r1 -> CF (r1 -> a) -> CF a
unCF1 a mf = CF $ \ctx -> getCF mf ctx a

unCF2 :: r1 -> r2 ->  CF (r1 -> r2 -> a) -> CF a
unCF2 a b mf = CF $ \ctx -> getCF mf ctx a b




--------------------------------------------------------------------------------
-- Combinators

-- | Lift a pure value into a Context functional. The 
-- DrawingContext is ignored.
--
-- > ans -> (ctx -> ans)
--
-- Without any other arguments, this is the same as the 'raise' 
-- combinator for raising into the Context functional. However, 
-- the /arity family/ of @wrap@ combinators is different.
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
promote1 f = CF $ \ctx a -> getCF (f a) ctx


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
promote2 df = CF $ \ctx a b -> getCF (df a b) ctx


-- | Lift a value into a Context functional.
--
-- > ans -> (ctx -> ans)
--
-- Essentially this is the @kestrel@ combinator - @const@ in 
-- Haskell, though due to newtype wrapping it is @pure@ from the
-- Applicative class.
--
raise :: a -> CF a
raise = pure


-- | Lift a one argument function into a Context functional.
--
-- This is Applicative\'s 'pure' with a specialized type 
-- signature. 
--
raise1 :: (r1 -> ans) -> CF (r1 -> ans) 
raise1 = pure

-- | Lift a two argument function into a Context functional.
--
-- This is Applicative\'s 'pure' with a specialized type 
-- signature.
--
raise2 :: (r1 -> r2 -> ans) -> CF (r1 -> r2 -> ans) 
raise2 = pure


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
static1 df = CF $ \ctx _ -> getCF df ctx


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
static2 df = CF $ \ctx a _ -> getCF df ctx a


-- | Complementary combinator to static2. 
--
-- This combinator raises a function two levels rather than one.
--
-- > (ctx -> ans) -> (ctx -> r1 -> r2 -> ans)
--
dblstatic :: CF ans -> CF (r1 -> r2 -> ans)
dblstatic df = CF $ \ctx _ _ -> getCF df ctx


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
bind df dk = CF $ \ctx -> let z = getCF df ctx in getCF (dk z) ctx


-- | Supply the output from the first function to the second 
-- function, /sharing/ the drawing context and the static 
-- argument @r1@.
--
-- > (ctx -> r1 -> a) -> (a -> ctx -> -> r1 -> ans) -> (ctx -> r1 -> ans)
-- 
bind1 :: CF (r1 -> a) -> (a -> CF (r1 -> ans)) -> CF (r1 -> ans)
bind1 df dk = CF $ \ctx a -> let z = getCF df ctx a in getCF (dk z) ctx a

-- | Supply the output from the first function to the second 
-- function, /sharing/ the DrawingContext and the two static 
-- arguments @r1@ and @r2@.
--
-- > (ctx -> r1 -> r2 -> a) -> (a -> ctx -> -> r1 -> r2 -> ans) -> (ctx -> r1 -> r2 -> ans)
-- 
bind2 :: CF (r1 -> r2 -> a) -> (a -> CF (r1 -> r2 -> ans)) 
      -> CF (r1 -> r2 -> ans)
bind2 df dk = CF $ \ctx a b -> 
    let z = getCF df ctx a b in getCF (dk z) ctx a b


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
situ1 df a = CF $ \ctx -> getCF df ctx a



-- | Supply the arguments to an arity 2 Conterxt functional so 
-- it can be /situated/. Typically this is supplying the start 
-- point and angle to a @LocThetaGraphic@ or @LocThetaImage@.
--
-- > (ctx -> r1 -> r2 -> ans) -> r1 -> r2 -> (ctx -> ans)
--
situ2 :: CF (r1 -> r2 -> ans) -> r1 -> r2 -> CF ans
situ2 df a b = CF $ \ctx -> getCF df ctx a b


-- | Apply the the functional produced by the first argument to
-- the value produced by the second.
--
-- > (ctx -> a -> ans) -> (ctx -> a) -> (ctx -> ans) 
--
apply :: CF (a -> ans) -> CF a -> CF ans
apply df da = CF $ \ctx -> getCF df ctx (getCF da ctx)


-- | Apply the the functional produced by the first argument to
-- the value produced by the second /sharing/ the context of the 
-- first functional argument @r1@ (usually a Point2) as well as 
-- the DrawingContext.
--
-- > (ctx -> r1 -> a -> ans) -> (ctx -> r1 -> a) -> (ctx -> r1 -> ans) 
--
apply1 :: CF (r1 -> a -> ans) -> CF (r1 -> a) -> CF (r1 -> ans)
apply1 df da = CF $ \ctx a -> getCF df ctx a (getCF da ctx a)


-- | Apply the the functional produced by the first argument to
-- the value produced by the second /sharing/ the context of the 
-- two functional arguments @r1@ and @r2@ as well as the 
-- DrawingContext.
--
-- > (ctx -> r1 -> r2 -> a -> ans) -> (ctx -> r1 -> r2 -> a) -> (ctx -> r1 -> r2 -> ans) 
--
apply2 :: CF (r1 -> r2 -> a -> ans) -> CF (r1 -> r2 -> a) 
       -> CF (r1 -> r2 -> ans)
apply2 df da = CF $ \ctx a b -> getCF df ctx a b (getCF da ctx a b)


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
prepro1 f mf = CF $ \ctx a -> getCF mf ctx (f a)


-- | Apply the static argument transfomers to their respective
-- static arguments /before/ applying the Context functional.
--
-- > (r1 -> a) -> (r2 -> b) -> (ctx -> a -> b -> ans) -> (ctx -> r1 -> r2 -> ans)
-- 
prepro2 :: (r1 -> a) -> (r2 -> b) -> CF (a -> b -> ans) -> CF (r1 -> r2 -> ans)
prepro2 f g mf = CF $ \ctx a b -> getCF mf ctx (f a) (g b)


-- | Apply the static argument transfomer to the first static
-- argument of a two static argument functional /before/ applying 
-- the Context functional.
--
-- > (r1 -> a) -> (ctx -> a -> r2 -> ans) -> (ctx -> r1 -> r2 -> ans)
-- 
prepro2a :: (r1 -> a) -> CF (a -> r2 -> ans) -> CF (r1 -> r2 -> ans)
prepro2a f mf = CF $ \ctx a b -> getCF mf ctx (f a) b

-- | Apply the static argument transfomer to the second static
-- argument of a two static argument functional /before/ applying 
-- the Context functional.
--
-- > (r2 -> a) -> (ctx -> r1 -> a -> ans) -> (ctx -> r1 -> r2 -> ans)
-- 
prepro2b :: (r2 -> a) -> CF (r1 -> a -> ans) -> CF (r1 -> r2 -> ans)
prepro2b f mf = CF $ \ctx a b -> getCF mf ctx a (f b)

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
postpro2 = postpro1 . fmap  


--------------------------------------------------------------------------------
-- Post-combiners

-- | Combine the results of the two Context Functions with the 
-- supplied operator.
--
-- > (a -> b -> ans) -> (ctx -> a) -> (ctx -> b) -> (ctx -> ans)
--
postcomb :: (a -> b -> ans) -> CF a -> CF b -> CF ans
postcomb op df dg = CF $ \ctx ->  getCF df ctx `op` getCF dg ctx

-- | Combine the results of the two one-static-argument Context 
-- Functions with the supplied operator.
--
-- > (a -> b -> ans) -> (ctx -> r1 -> a) -> (ctx -> r1 -> b) -> (ctx -> r1 -> ans)
--
postcomb1 :: (a -> b -> c) -> CF (r1 -> a) -> CF (r1 -> b) -> CF (r1 -> c)
postcomb1 op df dg = CF $ \ctx a -> getCF df ctx a `op` getCF dg ctx a


-- | Combine the results of the two two-static-argument Context 
-- Functions with the supplied operator.
--
-- > (a -> b -> ans) -> (ctx -> r1 -> a) -> (ctx -> r1 -> b) -> (ctx -> r1 -> ans)
--
postcomb2 :: (a -> b -> ans) -> CF (r1 -> r2 -> a) -> CF (r1 -> r2 -> b) 
          -> CF (r1 -> r2 -> ans)
postcomb2 op df dg = CF $ \ctx a b -> 
    getCF df ctx a b `op` getCF dg ctx a b



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
-- @ (ans -> ans -> ans) -> (ctx -> s1 -> (s1,ans)) @
-- @                     -> (ctx -> s1 -> (s1,ans)) -> (ctx -> s1 -> (s1,ans)) @
--
-- This models chaining start points together, which is the model
-- PostScript uses for text output when succesively calling the 
-- @show@ operator.
-- 
accumulate1 :: (ans -> ans -> ans) 
            -> CF (s1 -> (s1,ans)) -> CF (s1 -> (s1,ans)) 
            -> CF (s1 -> (s1,ans)) 
accumulate1 op f g = CF $ \ctx s -> 
                        let (s1,a1) = getCF f ctx s
                            (s2,a2) = getCF g ctx s1
                        in (s2, a1 `op` a2)



-- | Arity two version of accumulate1 - this is not expected to be
-- useful!
--
-- @ (ans -> ans -> ans) -> (ctx -> s1 -> -> s2 (s1,s2,ans)) @
-- @     -> (ctx -> s1 -> s2 -> (s1,s2,ans)) @
-- @     -> (ctx -> s1 -> s2 -> (s1,s2,ans)) @
--
accumulate2 :: (ans -> ans -> ans) 
            -> CF (s1 -> s2 -> (s1,s2,ans)) 
            -> CF (s1 -> s2 -> (s1,s2,ans)) 
            -> CF (s1 -> s2 -> (s1,s2,ans)) 
accumulate2 op f g = CF $ \ctx s t -> 
                        let (s1,t1,a1) = getCF f ctx s t
                            (s2,t2,a2) = getCF g ctx s1 t1
                        in (s2, t2, a1 `op` a2)





--------------------------------------------------------------------------------
{-
instance CenterAnchor a => CenterAnchor (CF a) where
  center = postpro center
-}