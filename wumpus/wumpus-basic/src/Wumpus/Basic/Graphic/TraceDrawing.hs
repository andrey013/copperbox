{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Graphic.TraceDrawing
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC 
--
-- Drawing with /trace/ - a Writer like monad collecting 
-- intermediate graphics - and /drawing context/ - a reader monad
-- of attributes - font_face, fill_colour etc.
--
--------------------------------------------------------------------------------

module Wumpus.Basic.Graphic.TraceDrawing
  (

    TraceDrawing
  , TraceDrawingT
  , runTraceDrawing
  , execTraceDrawing
  , evalTraceDrawing
  , runTraceDrawingT
  , execTraceDrawingT
  , evalTraceDrawingT

  , runFdcTraceDrawing
  , execFdcTraceDrawing
  , runFdcTraceDrawingT
  , execFdcTraceDrawingT

  , liftToPictureU
  , liftToPictureMb
  , mbPictureU
 

  , query
  , draw
  , xdraw
  , drawi
  , drawi_
  , xdrawi
  , xdrawi_

  , node
  , nodei

  ) where


import Wumpus.Basic.Graphic.Base
import Wumpus.Basic.Graphic.Drawing
import Wumpus.Basic.Graphic.DrawingContext
import Wumpus.Basic.Graphic.GraphicTypes

import Wumpus.Core                              -- package: wumpus-core

import Control.Applicative
import Control.Monad
import Data.Monoid



-- Note - TraceDrawing run \once\ - it is supplied with the starting
-- environment (DrawingContext) and returns a Picture.
--
-- Other Wumpus monads (e.g. Turtle) will typically be run inside
-- the TraceDrawing monad as a local effect, rather than built into a 
-- transformer stack.
--


newtype TraceDrawing u a   = TraceDrawing { 
          getTraceDrawing :: DrawingContext -> (a, HPrim u) }

newtype TraceDrawingT u m a = TraceDrawingT { 
          getTraceDrawingT :: DrawingContext -> m (a, HPrim u) }



type instance MonUnit (TraceDrawing u) = u
type instance MonUnit (TraceDrawingT u m) = u



-- Functor

instance Functor (TraceDrawing u) where
  fmap f ma = TraceDrawing $ \ctx -> 
                let (a,w) = getTraceDrawing ma ctx in (f a,w)


instance Monad m => Functor (TraceDrawingT u m) where
  fmap f ma = TraceDrawingT $ \ctx -> 
                getTraceDrawingT ma ctx >>= \(a,w) -> return (f a,w)



-- Applicative

instance Applicative (TraceDrawing u) where
  pure a    = TraceDrawing $ \_   -> (a, mempty)
  mf <*> ma = TraceDrawing $ \ctx -> 
                let (f,w1) = getTraceDrawing mf ctx
                    (a,w2) = getTraceDrawing ma ctx
                in (f a, w1 `mappend` w2)


instance Monad m => Applicative (TraceDrawingT u m) where
  pure a    = TraceDrawingT $ \_   -> return (a,mempty)
  mf <*> ma = TraceDrawingT $ \ctx -> 
                getTraceDrawingT mf ctx >>= \(f,w1) ->
                getTraceDrawingT ma ctx >>= \(a,w2) ->
                return (f a, w1 `mappend` w2)

-- Monad

instance Monad (TraceDrawing u) where
  return a  = TraceDrawing $ \_   -> (a, mempty)
  ma >>= k  = TraceDrawing $ \ctx -> 
                let (a,w1) = getTraceDrawing ma ctx
                    (b,w2) = (getTraceDrawing . k) a ctx
                in (b,w1 `mappend` w2)
                               



instance Monad m => Monad (TraceDrawingT u m) where
  return a  = TraceDrawingT $ \_   -> return (a, mempty)
  ma >>= k  = TraceDrawingT $ \ctx -> 
                getTraceDrawingT ma ctx      >>= \(a,w1) ->
                (getTraceDrawingT . k) a ctx >>= \(b,w2) -> 
                return (b, w1 `mappend` w2)
                                 



-- TraceM 
--
-- Note -  @ state `mappend` a @ means the first expression in a 
-- monadic drawing is the first element in the output file. It is
-- also \*\* at the back \*\* in the the Z-Order.
--
-- Some control over the Z-Order, possibly by adding /layers/ to 
-- the drawing model would be valuable. 
-- 

instance TraceM (TraceDrawing u) where
  trace a = TraceDrawing $ \_ -> ((), a)


instance Monad m => TraceM (TraceDrawingT u m) where
  trace a = TraceDrawingT $ \_ -> return ((), a)



-- DrawingCtxM

instance DrawingCtxM (TraceDrawing u) where
  askDC           = TraceDrawing $ \ctx -> (ctx, mempty)
  localize upd ma = TraceDrawing $ \ctx -> getTraceDrawing ma (upd ctx)



instance Monad m => DrawingCtxM (TraceDrawingT u m) where
  askDC           = TraceDrawingT $ \ctx -> return (ctx,mempty)
  localize upd ma = TraceDrawingT $ \ctx -> getTraceDrawingT ma (upd ctx)



-- Note - the result type of runTraceDrawing and friends needs more 
-- thought and may change. 
--
-- Possibly a wrapped HPrim that only supports concat and safe
-- extraction is best.
--
-- Or it could generate a picture, but then separate drawings
-- need the picture combinators to put them together. 
-- 


runTraceDrawing :: DrawingContext -> TraceDrawing u a -> (a, HPrim u)
runTraceDrawing ctx ma = getTraceDrawing ma ctx

-- | Run the drawing returning only the output it produces, drop
-- any answer from the monadic computation.
--
execTraceDrawing :: DrawingContext -> TraceDrawing u a -> HPrim u
execTraceDrawing ctx ma = snd $ runTraceDrawing ctx ma

-- | Run the drawing ignoring the output it produces, return the 
-- answer from the monadic computation.
--
-- Note - this useful for testing, generally one would want the 
-- opposite behaviour (return the drawing, ignore than the 
-- answer).
-- 
evalTraceDrawing :: DrawingContext -> TraceDrawing u a -> a
evalTraceDrawing ctx ma = fst $ runTraceDrawing ctx ma



runTraceDrawingT :: Monad m 
                 => DrawingContext -> TraceDrawingT u m a -> m (a, HPrim u) 
runTraceDrawingT ctx ma = getTraceDrawingT ma ctx

execTraceDrawingT :: Monad m 
                  => DrawingContext -> TraceDrawingT u m a -> m (HPrim u)
execTraceDrawingT ctx ma = liftM snd $ runTraceDrawingT ctx ma


evalTraceDrawingT :: Monad m 
                  => DrawingContext -> TraceDrawingT u m a -> m a
evalTraceDrawingT ctx ma = liftM fst $ runTraceDrawingT ctx ma



-- | Run the TraceDrawing generating a Picture /within/ a 
-- \"font delta context\" using the font-family and font-size 
-- from the intial TraceDrawingContext.
--
-- Using a /font delta context/ can reduce the code size of the
-- generated SVG file (PostScript ignores the FDC).
--
runFdcTraceDrawing :: (Real u, Floating u, FromPtSize u)
                   => DrawingContext -> TraceDrawing u a 
                   -> (a, Maybe (Picture u))
runFdcTraceDrawing ctx ma = 
    let (a,hp) = runTraceDrawing ctx ma
        ps     = hprimToList hp
        fdc    = font_props ctx
    in if null ps then (a, Nothing)
                 else (a, Just $ fontDeltaContext fdc $ frame ps)

-- | /exec/ version of 'runFdcContext'.
--
execFdcTraceDrawing :: (Real u, Floating u, FromPtSize u)
                    => DrawingContext -> TraceDrawing u a 
                    -> Maybe (Picture u)
execFdcTraceDrawing ctx ma = snd $ runFdcTraceDrawing ctx ma

-- | Transformer version of 'runFdcTraceDrawing'.
--
runFdcTraceDrawingT :: (Real u, Floating u, FromPtSize u, Monad m)
                    => DrawingContext -> TraceDrawingT u m a 
                    -> m (a, Maybe (Picture u))
runFdcTraceDrawingT ctx ma = 
    runTraceDrawingT ctx ma >>= \(a,hp) -> 
    let ps     = hprimToList hp
        fdc    = font_props ctx
    in if null ps then return (a, Nothing)
                  else return (a, Just $ fontDeltaContext fdc $ frame ps)
  
-- | Transformer version of 'execFdcTraceDrawing'.
--       
execFdcTraceDrawingT :: (Real u, Floating u, FromPtSize u, Monad m)
                     => DrawingContext -> TraceDrawingT u m a 
                     -> m (Maybe (Picture u))
execFdcTraceDrawingT ctx ma = liftM snd $ runFdcTraceDrawingT ctx ma

-- | /Unsafe/ promotion of @HPrim@ to @Picture@.
--
-- If the HPrim is empty, a run-time error is thrown.
-- 
liftToPictureU :: (Real u, Floating u, FromPtSize u) => HPrim u -> Picture u
liftToPictureU hf = 
    let prims = hprimToList hf in if null prims then errK else frame prims
  where
    errK = error "toPictureU - empty prims list."

-- | /Safe/ promotion of @HPrim@ to @(Maybe Picture)@.
--
-- If the HPrim is empty, then @Nothing@ is returned.
-- 
liftToPictureMb :: (Real u, Floating u, FromPtSize u) 
                => HPrim u -> Maybe (Picture u)
liftToPictureMb hf = let prims = hprimToList hf in 
    if null prims then Nothing else Just (frame prims)



-- | /Unsafe/ promotion of @(Maybe Picture)@ to @Picture@.
--
-- This is equivalent to:
--
-- > fromMaybe (error "empty") $ pic
--
-- This function is solely a convenience, using it saves one 
-- import and a few characters.
--
-- If the supplied value is @Nothing@ a run-time error is thrown.
-- 
mbPictureU :: (Real u, Floating u, FromPtSize u) 
           => Maybe (Picture u) -> Picture u
mbPictureU Nothing  = error "mbPictureU - empty picture."
mbPictureU (Just a) = a

-- Note - need an equivalent to Parsec\`s parseTest that provides
-- a very simple way to run graphics without concern for return 
-- type or initial drawing context.

--------------------------------------------------------------------------------

query :: DrawingCtxM m => Drawing a -> m a
query df = askDC >>= \ctx -> return $ runDrawing ctx df


-- | Draw a Graphic taking the drawing style from the 
-- /drawing context/. 
--
-- This operation is analogeous to @tell@ in a Writer monad.
-- 
draw :: (TraceM m, DrawingCtxM m, u ~ MonUnit m) => Graphic u -> m ()
draw gf = askDC >>= \ctx -> trace (collectH $ runGraphic ctx gf)

-- | Hyperlink version of 'draw'.
--
xdraw :: (TraceM m, DrawingCtxM m, u ~ MonUnit m) 
      => XLink -> Graphic u -> m ()
xdraw xl gf = draw (xlinkGraphic xl gf)



-- | Draw an Image taking the drawing style from the 
-- /drawing context/. 
--
-- The graphic representation of the Image is drawn in the Trace 
-- monad, and the result is returned.
-- 
drawi :: (TraceM m, DrawingCtxM m, u ~ MonUnit m) => Image u a -> m a
drawi img = askDC >>= \ctx -> 
            let (a,o) = runImage ctx img in trace (collectH o) >> return a

-- | Forgetful 'drawi'.
--
drawi_ ::  (TraceM m, DrawingCtxM m, u ~ MonUnit m) => Image u a -> m ()
drawi_ img = drawi img >> return ()


-- | Hyperlink version of 'drawi'.
--
xdrawi ::  (TraceM m, DrawingCtxM m, u ~ MonUnit m) 
       => XLink -> Image u a -> m a
xdrawi xl img = drawi (xlinkImage xl img)


-- | Forgetful 'xdrawi'.
--
xdrawi_ ::  (TraceM m, DrawingCtxM m, u ~ MonUnit m) 
        => XLink -> Image u a -> m ()
xdrawi_ xl img = xdrawi xl img >> return ()




node :: (TraceM m, DrawingCtxM m, PointSupplyM m, u ~ MonUnit m) 
     => LocGraphic u -> m ()
node gf = askDC    >>= \ctx -> 
          position >>= \pt  -> 
          let f    = runDrawing ctx gf in trace (collectH $ f pt)


nodei :: (TraceM m, DrawingCtxM m, PointSupplyM m, u ~ MonUnit m) 
     => LocImage u a -> m a
nodei imgL = askDC    >>= \ctx -> 
             position >>= \pt  -> 
             let (a,o) = runLocImage ctx imgL pt
             in trace (collectH o) >> return a

