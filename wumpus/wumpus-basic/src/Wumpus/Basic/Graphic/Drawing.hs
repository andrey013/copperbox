{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Graphic.Drawing
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  highly unstable
-- Portability :  GHC 
--
-- The primary Drawing type and base combinators to manipulate it. 
--
--------------------------------------------------------------------------------

module Wumpus.Basic.Graphic.Drawing
  (

    Drawing

  , LocDrawing
  , LocThetaDrawing
  , ConnectorDrawing
  , DLocDrawing
  , DLocThetaDrawing
  , DConnectorDrawing



  -- * Run functions
  , runDrawing

  -- * Extractors
  , drawingCtx
  , queryDrawing
  , locCtx
  , locPoint
  , locThetaCtx
  , locThetaPoint
  , locThetaAng
  , connCtx
  , connStart
  , connEnd

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


  , cardinalprime


  -- * Pre-transformers
  , prepro1
  , prepro2
  , prepro2a
  , prepro2b

  -- * Post-transformers
  , postpro
  , postpro1
  , postpro2

  , cxpost1

  -- * Post-combiners
  , postcomb
  , postcomb1
  , postcomb2

  , accumulate1
  , accumulate2


  ) where

import Wumpus.Basic.Graphic.Base
import Wumpus.Basic.Graphic.DrawingContext

import Wumpus.Core                      -- package: wumpus-core


import Control.Applicative
import Data.Monoid


--------------------------------------------------------------------------------
--

-- | Drawings in Wumpus-Basic have an implicit /graphics state/ 
-- the @DrawingContext@, the most primitive building block is 
-- a function from the DrawingContext to some polymorphic answer.
-- 
-- This functional type is represented concretely as @Drawing@.
-- 
-- > Drawing :: DrawingContext -> a 
--
newtype Drawing a = Drawing { getDrawing :: DrawingContext -> a }


type LocDrawing        u a = Drawing (Point2 u -> a)

type LocThetaDrawing   u a = LocDrawing u (Radian -> a)

type ConnectorDrawing  u a = LocDrawing u (Point2 u -> a)



type DLocDrawing a         = LocDrawing       Double a
type DLocThetaDrawing a    = LocThetaDrawing  Double a
type DConnectorDrawing a   = ConnectorDrawing Double a








--------------------------------------------------------------------------------
-- Drawing instances

instance Functor Drawing where
  fmap f ma = Drawing $ \ctx -> f $ getDrawing ma ctx 


instance OPlus a => OPlus (Drawing a)  where
  fa `oplus` fb = Drawing $ \ctx -> 
                      getDrawing fa ctx `oplus` getDrawing fb ctx

-- The monoid instance seems sensible...
--
instance Monoid a => Monoid (Drawing a) where 
  mempty          = Drawing $ \_   -> mempty
  fa `mappend` fb = Drawing $ \ctx -> 
                      getDrawing fa ctx `mappend` getDrawing fb ctx

-- Applicative

instance Applicative Drawing where
  pure a    = Drawing $ \_   -> a
  mf <*> ma = Drawing $ \ctx -> let f = getDrawing mf ctx
                                    a = getDrawing ma ctx
                                in f a
-- Monad 

instance Monad Drawing where
  return a  = Drawing $ \_   -> a
  ma >>= k  = Drawing $ \ctx -> let a = getDrawing ma ctx
                                in (getDrawing . k) a ctx 


instance DrawingCtxM Drawing where
  askDC           = Drawing $ \ctx -> ctx
  localize upd df = Drawing $ \ctx -> getDrawing df (upd ctx)
  



--------------------------------------------------------------------------------
-- Run functions



-- | Run a /Drawing Function/ with the supplied /Drawing Context/.
--
runDrawing :: DrawingContext -> Drawing a -> a
runDrawing ctx df = getDrawing df ctx


--------------------------------------------------------------------------------
-- extractors 

-- | Extract the drawing context from a Drawing.
--
-- > (ctx -> ctx)
-- 
drawingCtx      :: Drawing DrawingContext
drawingCtx      = Drawing $ \ctx -> ctx

-- | Apply the projection function to the drawing context.
--
-- > (ctx -> a) -> (ctx -> a)
--
queryDrawing    :: (DrawingContext -> a) -> Drawing a
queryDrawing f  = Drawing $ \ctx -> f ctx


-- | Extract the drawing context from a LocDrawing.
--
-- > (ctx -> pt -> ctx)
--
locCtx          :: LocDrawing u DrawingContext
locCtx          = Drawing $ \ctx _  -> ctx

-- | Extract the /start/ point from a LocDrawing.
--
-- > (ctx -> pt -> pt)
--
locPoint        :: LocDrawing u (Point2 u)
locPoint        = Drawing $ \_ pt -> pt


-- | Extract the drawing context from a LocThetaDrawing.
--
-- > (ctx -> pt -> ang -> ctx)
--
locThetaCtx     :: LocThetaDrawing u DrawingContext
locThetaCtx     = Drawing $ \ctx _ _ -> ctx


-- | Extract the /start/ point from a LocThetaDrawing.
--
-- > (ctx -> pt -> ang -> pt)
--
locThetaPoint   :: LocThetaDrawing u (Point2 u)
locThetaPoint   = Drawing $ \_ pt _ -> pt

-- | Extract the angle from a LocThetaDrawing.
--
-- > (ctx -> pt -> ang -> ang)
--
locThetaAng     :: LocThetaDrawing u Radian
locThetaAng     = Drawing $ \_ _ ang -> ang

-- | Extract the drawing context from a ConnectorDrawing.
--
-- > (ctx -> pt1 -> pt2 -> ctx)
--
connCtx         :: ConnectorDrawing u DrawingContext
connCtx         = Drawing $ \ctx _ _ -> ctx

-- | Extract the start point from a ConnectorDrawing.
--
-- > (ctx -> pt1 -> pt2 -> pt1)
--
connStart       :: ConnectorDrawing u (Point2 u) 
connStart       = Drawing $ \_ pt _ -> pt

-- | Extract the end point from a ConnectorDrawing.
--
-- > (ctx -> pt1 -> pt2 -> pt2)
--
connEnd         :: ConnectorDrawing u (Point2 u) 
connEnd         = Drawing $ \_ _ pt -> pt

--------------------------------------------------------------------------------
-- Combinators

-- | Lift a pure value into a Drawing. The Drawing Context is 
-- /ignored/.
--
-- > ans -> (ctx -> ans)
--
-- This is the same as the 'raise' combinator at when raising into
-- the drawing Context. But the /arity family/ of @wrap@ 
-- combinators is different.
--
wrap :: a -> Drawing a
wrap = pure


-- | Lift a pure value into a Drawing, ignoring both the Drawing 
-- Context and the functional argument (e.g. start point for a 
-- @LocDrawing@).
--
-- > ans -> (ctx -> r1 -> ans)
--
wrap1 :: a -> Drawing (r1 -> a)
wrap1 = pure . pure

-- | Lift a pure value into a Drawing, ignoring both the Drawing 
-- Context and the two functional arguments (e.g. start point and 
-- angle for a @LocThetaDrawing@).
--
-- > ans -> (ctx -> r1 -> r2 -> ans)
--
wrap2 :: a -> Drawing (r1 -> r2 -> a)
wrap2 = pure . pure . pure

   

-- | Promote a one argument drawing function into a one argument 
-- drawing /functional/.
--
-- The type signature is probably more illustrative of the 
-- operation than this description:
--
-- > (r1 -> ctx -> ans) -> (ctx -> r1 -> ans)
--
-- This is essentially the @cardinal@ combinator - @flip@
-- in Haskell.
--
promote1 :: (r1 -> Drawing ans) -> Drawing (r1 -> ans)
promote1 f = Drawing $ \ctx a -> getDrawing (f a) ctx


-- | Promote a two argument drawing function into a two argument 
-- drawing /functional/.
--
-- The type signature is probably more illustrative of the 
-- operation than this description:
--
-- > (r1 -> r2 -> ctx -> ans) -> (ctx -> r1 -> r2 -> ans)
--
promote2 :: (r1 -> r2 -> Drawing ans) -> Drawing (r1 -> r2 -> ans)
promote2 df = Drawing $ \ctx a b -> getDrawing (df a b) ctx


-- | Lift a value into a Drawing.
--
-- > ans -> (ctx -> ans)
--
-- Essentially this is the @kestrel@ combinator - @const@ in 
-- Haskell, though due to newtype wrapping it is @pure@ from the
-- Applicative class.
--
raise :: a -> Drawing a
raise = pure


-- | Lift a one argument function into a Drawing /functional/.
--
-- This is Applicative\'s 'pure' with a specialized type 
-- signature. 
--
raise1 :: (r1 -> ans) -> Drawing (r1 -> ans) 
raise1 = pure

-- | Lift a two argument function into a Drawing /functional/.
--
-- This is Applicative\'s 'pure' with a specialized type 
-- signature.
--
raise2 :: (r1 -> r2 -> ans) -> Drawing (r1 -> r2 -> ans) 
raise2 = pure


-- | Extend the arity of a /drawing functional/, the original 
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
static1 :: Drawing ans -> Drawing (r1 -> ans)
static1 df = Drawing $ \ctx _ -> getDrawing df ctx


-- | Extend the arity of a /drawing functional/, the original 
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
static2 :: Drawing (r1 -> ans) -> Drawing (r1 -> r2 -> ans)
static2 df = Drawing $ \ctx a _ -> getDrawing df ctx a


-- | Complementary combinator to static2. 
--
-- This combinator raises a function two levels rather than one.
--
-- > (ctx -> ans) -> (ctx -> r1 -> r2 -> ans)
--
dblstatic :: Drawing ans -> Drawing (r1 -> r2 -> ans)
dblstatic df = Drawing $ \ctx _ _ -> getDrawing df ctx





-- | Supply the output from the first function to the second 
-- function.
--
-- This is just monadic bind - specialized to the Drawing 
-- functional type.
--
-- > (ctx -> a) -> (a -> ctx -> ans) -> (ctx -> ans)
-- 
bind :: Drawing a -> (a -> Drawing ans) -> Drawing ans
bind df dk = Drawing $ \ctx -> 
    let z = getDrawing df ctx in getDrawing (dk z) ctx


-- | Supply the output from the first function to the second 
-- function, /sharing/ the drawing context and the static 
-- argument @r1@.
--
-- > (ctx -> r1 -> a) -> (a -> ctx -> -> r1 -> ans) -> (ctx -> r1 -> ans)
-- 
bind1 :: Drawing (r1 -> a) -> (a -> Drawing (r1 -> ans)) -> Drawing (r1 -> ans)
bind1 df dk = Drawing $ \ctx a ->
    let z = getDrawing df ctx a in getDrawing (dk z) ctx a

-- | Supply the output from the first function to the second 
-- function, /sharing/ the drawing context and the two static 
-- arguments @r1@ and @r2@.
--
-- > (ctx -> r1 -> r2 -> a) -> (a -> ctx -> -> r1 -> r2 -> ans) -> (ctx -> r1 -> r2 -> ans)
-- 
bind2 :: Drawing (r1 -> r2 -> a) -> (a -> Drawing (r1 -> r2 -> ans)) 
      -> Drawing (r1 -> r2 -> ans)
bind2 df dk = Drawing $ \ctx a b -> 
    let z = getDrawing df ctx a b in getDrawing (dk z) ctx a b


-- idstar  :: (r1 -> r2 -> ans) -> r1 -> r2 -> ans

-- | Supply the arguments to an arity 1 drawing so it can be 
-- /situated/. Typically this is supplying the start point to a 
-- @LocGraphic@ or @LocImage@.
--
-- > (ctx -> r1 -> ans) -> r1 -> (ctx -> ans)
--
-- This is equivalent to the @id**@ combinator.
--
situ1 :: Drawing (r1 -> ans) -> r1 -> Drawing ans
situ1 df a = Drawing $ \ctx -> getDrawing df ctx a



-- | Supply the arguments to an arity 2 drawing so it can be 
-- /situated/. Typically this is supplying the start point and 
-- angle to a @LocThetaGraphic@ or @LocThetaImage@.
--
-- > (ctx -> r1 -> r2 -> ans) -> r1 -> r2 -> (ctx -> ans)
--
situ2 :: Drawing (r1 -> r2 -> ans) -> r1 -> r2 -> Drawing ans
situ2 df a b = Drawing $ \ctx -> getDrawing df ctx a b


-- | Apply the the functional produced by the first argument to
-- the value produced by the second.
--
-- > (ctx -> a -> ans) -> (ctx -> a) -> (ctx -> ans) 
--
apply :: Drawing (a -> ans) -> Drawing a -> Drawing ans
apply df da = Drawing $ \ctx -> getDrawing df ctx (getDrawing da ctx)


-- | Apply the the functional produced by the first argument to
-- the value produced by the second /sharing/ the context of the 
-- first functional argument @r1@ (usually a Point2) as well as 
-- the drawing context.
--
-- > (ctx -> r1 -> a -> ans) -> (ctx -> r1 -> a) -> (ctx -> r1 -> ans) 
--
apply1 :: Drawing (r1 -> a -> ans) -> Drawing (r1 -> a) -> Drawing (r1 -> ans)
apply1 df da = Drawing $ \ctx a -> getDrawing df ctx a (getDrawing da ctx a)


-- | Apply the the functional produced by the first argument to
-- the value produced by the second /sharing/ the context of the 
-- two functional arguments @r1@ and @r2@ as well as the drawing 
-- context.
--
-- > (ctx -> r1 -> r2 -> a -> ans) -> (ctx -> r1 -> r2 -> a) -> (ctx -> r1 -> r2 -> ans) 
--
apply2 :: Drawing (r1 -> r2 -> a -> ans) -> Drawing (r1 -> r2 -> a) 
       -> Drawing (r1 -> r2 -> ans)
apply2 df da = Drawing $ \ctx a b -> 
                 getDrawing df ctx a b (getDrawing da ctx a b)

-- These combinators haven\'t been looked at systemmatically vis arity...





-- cardinal'  :: (a -> r1 -> ans) -> (r2 -> a) -> r1 -> r2 -> ans

-- | Note - this combinator seems useful, but perhaps it is not 
-- /primitive/ and it may be removed or renamed. 
--
-- (a -> ctx -> ans) -> (r1 -> a) -> (ctx -> r1 -> ans)
--
-- This is a /Cardinal-prime/ combinator.
--
cardinalprime :: (a -> Drawing ans) -> (r1 -> a) -> Drawing (r1 -> ans)
cardinalprime f g = promote1 f `cxpost1` (raise g)







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
-- Drawing newtype and TraceDrawing monads.
--
-- > localizeDR :: (DrawingContext -> DrawingContext) -> Drawing a -> Drawing a
-- > localizeDR = localize
--
-- Figuartively speaking this is an opposite of the fmap function 
-- instance. Instead of (post-) transforming the output, ... 
-- (pre-) transforms  the input.
-- 
-- Due to Drawing functionals being stacked in a particular way
-- @localize@ always pre-transforms the DrawingContext regardless
-- of the arity of the functional:
--
-- > localizeDR2 :: (DrawingContext -> DrawingContext) 
-- >             -> Drawing (r -> a) -> Drawing (r -> a)
-- > localizeDR2 = localize
-- 


-- | Apply the static argument transfomer @(r1 -> a)@ to the 
-- static argument /before/ applying the drawing functional.
--
-- > (r1 -> a) -> (ctx -> a -> ans) -> (ctx -> r1 -> ans)
-- 
prepro1 :: (r1 -> a) -> Drawing (a -> ans) -> Drawing (r1 -> ans)
prepro1 f mf = Drawing $ \ctx a -> getDrawing mf ctx (f a)


-- | Apply the static argument transfomers to their respective
-- static arguments /before/ applying the drawing functional.
--
-- > (r1 -> a) -> (r2 -> b) -> (ctx -> a -> b -> ans) -> (ctx -> r1 -> r2 -> ans)
-- 
prepro2 :: (r1 -> a) -> (r2 -> b) -> Drawing (a -> b -> ans) 
        -> Drawing (r1 -> r2 -> ans)
prepro2 f g mf = Drawing $ \ctx a b -> getDrawing mf ctx (f a) (g b)


-- | Apply the static argument transfomer to the first static
-- argument of a two static argument functional /before/ applying 
-- the drawing functional.
--
-- > (r1 -> a) -> (ctx -> a -> r2 -> ans) -> (ctx -> r1 -> r2 -> ans)
-- 
prepro2a :: (r1 -> a) -> Drawing (a -> r2 -> ans) -> Drawing (r1 -> r2 -> ans)
prepro2a f mf = Drawing $ \ctx a b -> getDrawing mf ctx (f a) b

-- | Apply the static argument transfomer to the second static
-- argument of a two static argument functional /before/ applying 
-- the drawing functional.
--
-- > (r2 -> a) -> (ctx -> r1 -> a -> ans) -> (ctx -> r1 -> r2 -> ans)
-- 
prepro2b :: (r2 -> a) -> Drawing (r1 -> a -> ans) -> Drawing (r1 -> r2 -> ans)
prepro2b f mf = Drawing $ \ctx a b -> getDrawing mf ctx a (f b)

------------------------------------------------------------------------------
-- Post-transfomers

-- | Apply the post-transformer to the result of the drawing 
-- functional.
--
-- > (a -> ans) -> (ctx -> a) -> (ctx -> ans) 
--
postpro :: (a -> ans) -> Drawing a -> Drawing ans
postpro = fmap

-- | Apply the post-transformer to the result of the drawing 
-- functional. Version for one static argument.
--
-- Note - the drawing context is always present so it is never 
-- counted as a static argument.
--
-- > (a -> ans) -> (ctx -> r1 -> a) -> (ctx -> r1 -> ans) 
--
postpro1 :: (a -> ans) -> Drawing (r1 -> a) -> Drawing (r1 -> ans)
postpro1 = postpro . fmap  

-- | Apply the post-transformer to the result of the drawing 
-- functional. Version for two static arguments.
--
-- Note - the drawing context is always present so it is never 
-- counted as a static argument.
--
--
-- > (a -> ans) -> (ctx -> r1 -> r2 -> a) -> (ctx -> r1 -> r2 -> ans) 
--
postpro2 :: (a -> ans) -> Drawing (r1 -> r2 -> a) -> Drawing (r1 -> r2 -> ans)
postpro2 = postpro1 . fmap  



-- | Post-process the result of an one-static-argument drawing
-- with a /contextual/ transformer. 
--
-- > (ctx -> a -> ans) -> (ctx -> r1 -> a) -> (ctx -> r1 -> ans)
-- 
cxpost1 :: Drawing (a -> ans) -> Drawing (r1 -> a) -> Drawing (r1 -> ans)
cxpost1 f g = Drawing $ \ctx a -> getDrawing f ctx (getDrawing g ctx a)

--------------------------------------------------------------------------------
-- Post-combiners

-- | Combine the results of the two drawings with the supplied 
-- operator.
--
-- > (a -> b -> ans) -> (ctx -> a) -> (ctx -> b) -> (ctx -> ans)
--
postcomb :: (a -> b -> ans) -> Drawing a -> Drawing b -> Drawing ans
postcomb op df dg = Drawing $ \ctx -> 
    getDrawing df ctx `op` getDrawing dg ctx

-- | Combine the results of the two one-static-argument drawings 
-- with the supplied operator.
--
-- > (a -> b -> ans) -> (ctx -> r1 -> a) -> (ctx -> r1 -> b) -> (ctx -> r1 -> ans)
--
postcomb1 :: (a -> b -> c) -> Drawing (r1 -> a) -> Drawing (r1 -> b) 
          -> Drawing (r1 -> c)
postcomb1 op df dg = Drawing $ \ctx a -> 
    getDrawing df ctx a `op` getDrawing dg ctx a


-- | Combine the results of the two two-static-argument drawings 
-- with the supplied operator.
--
-- > (a -> b -> ans) -> (ctx -> r1 -> a) -> (ctx -> r1 -> b) -> (ctx -> r1 -> ans)
--
postcomb2 :: (a -> b -> ans) -> Drawing (r1 -> r2 -> a) -> Drawing (r1 -> r2 -> b) 
          -> Drawing (r1 -> r2 -> ans)
postcomb2 op df dg = Drawing $ \ctx a b -> 
    getDrawing df ctx a b `op` getDrawing dg ctx a b



-- | Iteration combinator - the initial argument @s1@ is not 
-- shared bewteen the drawings.
--
-- Evaluate the first drawing with the drawing context and the 
-- /initial state/ @st0@. The result of the evaluation is a new 
-- /state/ @st1@ and and answer @a1@. 
--
-- Evaluate the second drawing with the drawing context and the
-- new state @st1@, producing a new state @s2@ and an answer @a2@
--
-- Return the result of combinig the answers with @op :: (ans -> ans -> ans)@
-- and the second state @s2@.
--
-- @ (ans -> ans -> ans) -> (ctx -> s1 -> (s1,ans)) @
-- @                     -> (ctx -> s1 -> (s1,ans)) -> (ctx -> s1 -> (s1,ans)) @
--
-- This models chaining start points together, shich is the model
-- PostScript uses for succesively calling the @show@ operator.
-- 
accumulate1 :: (ans -> ans -> ans) 
            -> Drawing (s1 -> (s1,ans)) -> Drawing (s1 -> (s1,ans)) 
            -> Drawing (s1 -> (s1,ans)) 
accumulate1 op f g = Drawing $ \ctx s -> 
                        let (s1,a1) = getDrawing f ctx s
                            (s2,a2) = getDrawing g ctx s1
                        in (s2, a1 `op` a2)



-- | Arity two version of accumulate1 - this is not expected to be
-- useful!
--
-- @ (ans -> ans -> ans) -> (ctx -> s1 -> -> s2 (s1,s2,ans)) @
-- @     -> (ctx -> s1 -> s2 -> (s1,s2,ans)) @
-- @     -> (ctx -> s1 -> s2 -> (s1,s2,ans)) @
--
accumulate2 :: (ans -> ans -> ans) 
            -> Drawing (s1 -> s2 -> (s1,s2,ans)) 
            -> Drawing (s1 -> s2 -> (s1,s2,ans)) 
            -> Drawing (s1 -> s2 -> (s1,s2,ans)) 
accumulate2 op f g = Drawing $ \ctx s t -> 
                        let (s1,t1,a1) = getDrawing f ctx s t
                            (s2,t2,a2) = getDrawing g ctx s1 t1
                        in (s2, t2, a1 `op` a2)






