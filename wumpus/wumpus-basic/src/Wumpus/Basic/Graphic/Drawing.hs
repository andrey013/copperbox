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
-- Base types for Drawing Objects, Graphics / Images (a Graphic 
-- that also returns an answer), etc.
-- 
-- \*\* WARNING \*\* - some names are expected to change.
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
  , locThetaAng
  , locThetaPoint
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


  , compose
  , cardinalprime


  -- * Pre-transformers
  , prepro1
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

drawingCtx      :: Drawing DrawingContext
drawingCtx      = Drawing $ \ctx -> ctx

queryDrawing    :: (DrawingContext -> a) -> Drawing a
queryDrawing f  = Drawing $ \ctx -> f ctx

locCtx          :: LocDrawing u DrawingContext
locCtx          = Drawing $ \ctx _  -> ctx

locPoint        :: LocDrawing u (Point2 u)
locPoint        = Drawing $ \_ pt -> pt


locThetaCtx     :: LocThetaDrawing u DrawingContext
locThetaCtx     = Drawing $ \ctx _ _ -> ctx

locThetaAng     :: LocThetaDrawing u Radian
locThetaAng     = Drawing $ \_ _ ang -> ang

locThetaPoint   :: LocThetaDrawing u (Point2 u)
locThetaPoint   = Drawing $ \_ pt _ -> pt


connCtx         :: ConnectorDrawing u DrawingContext
connCtx         = Drawing $ \ctx _ _ -> ctx

connStart       :: ConnectorDrawing u (Point2 u) 
connStart       = Drawing $ \_ pt _ -> pt

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
-- the drawing Context. But the /arity family/ of wrap combinators 
-- is different.
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
-- theta for a @LocThetaDrawing@).
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
-- Essentially this is the @kestrel@ combinator - @const@ in Haskell.
--
raise :: a -> Drawing a
raise = pure


-- | Lift a one argument function into a Drawing /functional/.
--
-- This is 'ctxFree' with a specialized type signature. 
--
raise1 :: (r1 -> ans) -> Drawing (r1 -> ans) 
raise1 = pure

-- | Lift a two argument function into a Drawing /functional/.
--
-- This is 'ctxFree' with a specialized type signature.
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






bind :: Drawing a -> (a -> Drawing ans) -> Drawing ans
bind df dk = Drawing $ \ctx -> 
    let z = getDrawing df ctx in getDrawing (dk z) ctx


bind1 :: Drawing (r1 -> a) -> (a -> Drawing (r1 -> ans)) -> Drawing (r1 -> ans)
bind1 df dk = Drawing $ \ctx a -> 
    let z = getDrawing df ctx a in getDrawing (dk z) ctx a

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



apply :: Drawing (a -> ans) -> Drawing a -> Drawing ans
apply df da = Drawing $ \ctx -> getDrawing df ctx (getDrawing da ctx)


apply1 :: Drawing (r1 -> a -> ans) -> Drawing (r1 -> a) -> Drawing (r1 -> ans)
apply1 df da = Drawing $ \ctx a -> getDrawing df ctx a (getDrawing da ctx a)


apply2 :: Drawing (r1 -> r2 -> a -> ans) -> Drawing (r1 -> r2 -> a) 
       -> Drawing (r1 -> r2 -> ans)
apply2 df da = Drawing $ \ctx a b -> 
                 getDrawing df ctx a b (getDrawing da ctx a b)

-- These combiantors haven\'t been looked at systemmatically vis arity...

compose :: Drawing (b -> c) -> Drawing (a -> b) -> Drawing (a -> c)
compose f g = Drawing $ \ctx a -> getDrawing f ctx (getDrawing g ctx a)



-- cardinal'  :: (a -> r1 -> ans) -> (r2 -> a) -> r1 -> r2 -> ans

-- | This is a /Cardinal-prime/
--
-- (b -> ctx -> c) -> (a -> b) -> ctx -> a -> c
--
cardinalprime :: (b -> Drawing c) -> (a -> b) -> Drawing (a -> c)
cardinalprime f g = promote1 f `compose` (raise g)







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

prepro1 :: (r1 -> a) -> Drawing (a -> ans) -> Drawing (r1 -> ans)
prepro1 f mf = Drawing $ \ctx a -> getDrawing mf ctx (f a)


prepro2a :: (r1 -> a) -> Drawing (a -> r2 -> b) -> Drawing (r1 -> r2 -> b)
prepro2a f mf = Drawing $ \ctx a b -> getDrawing mf ctx (f a) b

prepro2b :: (r2 -> a) -> Drawing (r1 -> a -> b) -> Drawing (r1 -> r2 -> b)
prepro2b f mf = Drawing $ \ctx a b -> getDrawing mf ctx a (f b)

------------------------------------------------------------------------------
-- Post-transfomers


postpro :: (a -> b) -> Drawing a -> Drawing b
postpro = fmap

postpro1 :: (a -> b) -> Drawing (r1 -> a) -> Drawing (r1 -> b)
postpro1 = postpro . fmap  

postpro2 :: (a -> b) -> Drawing (r1 -> r2 -> a) -> Drawing (r1 -> r2 -> b)
postpro2 = postpro1 . fmap  

--------------------------------------------------------------------------------
-- Post-combiners

-- (a -> b -> c) -> (ctx -> a) -> (ctx -> b) -> (ctx -> c)

postcomb :: (a -> b -> c) -> Drawing a -> Drawing b -> Drawing c
postcomb op df dg = Drawing $ \ctx -> 
    getDrawing df ctx `op` getDrawing dg ctx

postcomb1 :: (a -> b -> c) -> Drawing (r1 -> a) -> Drawing (r1 -> b) 
          -> Drawing (r1 -> c)
postcomb1 op df dg = Drawing $ \ctx a -> 
    getDrawing df ctx a `op` getDrawing dg ctx a


postcomb2 :: (a -> b -> c) -> Drawing (r1 -> r2 -> a) -> Drawing (r1 -> r2 -> b) 
          -> Drawing (r1 -> r2 -> c)
postcomb2 op df dg = Drawing $ \ctx a b -> 
    getDrawing df ctx a b `op` getDrawing dg ctx a b



-- | This models chaining start points together e.g. how 
-- PostScript text.
-- 
accumulate1 :: (a -> a -> a) 
            -> Drawing (r1 -> (r1,a)) -> Drawing (r1 -> (r1,a)) 
            -> Drawing (r1 -> (r1,a)) 
accumulate1 op f g = Drawing $ \ctx pt -> 
                        let (p1,a1) = getDrawing f ctx pt
                            (p2,a2) = getDrawing g ctx p1
                        in (p2, a1 `op` a2)



-- | Arity two version of accumulate1 - this is not expected to be
-- useful!
--
accumulate2 :: (a -> a -> a) 
            -> Drawing (r1 -> r2 -> (r1,r2,a)) 
            -> Drawing (r1 -> r2 -> (r1,r2,a)) 
            -> Drawing (r1 -> r2 -> (r1,r2,a)) 
accumulate2 op f g = Drawing $ \ctx pt r -> 
                        let (p1,r1,a1) = getDrawing f ctx pt r
                            (p2,r2,a2) = getDrawing g ctx p1 r1
                        in (p2, r2, a1 `op` a2)



--------------------------------------------------------------------------------
--




