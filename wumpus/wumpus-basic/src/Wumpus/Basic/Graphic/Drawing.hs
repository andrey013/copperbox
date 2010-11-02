{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Graphic.Drawing
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

module Wumpus.Basic.Graphic.Drawing
  (

    Drawing
  , DrawingT
  , runDrawing
  , execDrawing
  , evalDrawing
  , runDrawingT
  , execDrawingT
  , evalDrawingT

  , runFdcDrawing
  , execFdcDrawing
  , runFdcDrawingT
  , execFdcDrawingT

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

  , at

  , node
  , nodei

  ) where


import Wumpus.Basic.Graphic.Base
import Wumpus.Basic.Graphic.DrawingContext
import Wumpus.Basic.Graphic.Prim

import Wumpus.Core                              -- package: wumpus-core

import Control.Applicative
import Control.Monad
import Data.Monoid



-- Note - Drawing run \once\ - it is supplied with the starting
-- environment (DrawingContext) and returns a Picture.
--
-- Other Wumpus monads (e.g. Turtle) will typically be run inside
-- the Drawing monad as a local effect, rather than built into a 
-- transformer stack.
--


newtype Drawing u a   = Drawing { 
          getDrawing :: DrawingContext -> (a, HPrim u) }

newtype DrawingT u m a = DrawingT { 
          getDrawingT :: DrawingContext -> m (a, HPrim u) }



type instance MonUnit (Drawing u) = u
type instance MonUnit (DrawingT u m) = u



-- Functor

instance Functor (Drawing u) where
  fmap f ma = Drawing $ \ctx -> 
                let (a,w) = getDrawing ma ctx in (f a,w)


instance Monad m => Functor (DrawingT u m) where
  fmap f ma = DrawingT $ \ctx -> 
                getDrawingT ma ctx >>= \(a,w) -> return (f a,w)



-- Applicative

instance Applicative (Drawing u) where
  pure a    = Drawing $ \_   -> (a, mempty)
  mf <*> ma = Drawing $ \ctx -> let (f,w1) = getDrawing mf ctx
                                    (a,w2) = getDrawing ma ctx
                                in (f a, w1 `mappend` w2)


instance Monad m => Applicative (DrawingT u m) where
  pure a    = DrawingT $ \_   -> return (a,mempty)
  mf <*> ma = DrawingT $ \ctx -> getDrawingT mf ctx >>= \(f,w1) ->
                                 getDrawingT ma ctx >>= \(a,w2) ->
                                 return (f a, w1 `mappend` w2)

-- Monad

instance Monad (Drawing u) where
  return a  = Drawing $ \_   -> (a, mempty)
  ma >>= k  = Drawing $ \ctx -> let (a,w1) = getDrawing ma ctx
                                    (b,w2) = (getDrawing . k) a ctx
                                in (b,w1 `mappend` w2)
                               



instance Monad m => Monad (DrawingT u m) where
  return a  = DrawingT $ \_   -> return (a, mempty)
  ma >>= k  = DrawingT $ \ctx -> getDrawingT ma ctx      >>= \(a,w1) ->
                                 (getDrawingT . k) a ctx >>= \(b,w2) -> 
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

instance TraceM (Drawing u) where
  trace a = Drawing $ \_ -> ((), a)


instance Monad m => TraceM (DrawingT u m) where
  trace a = DrawingT $ \_ -> return ((), a)



-- DrawingCtxM

instance DrawingCtxM (Drawing u) where
  askDC           = Drawing $ \ctx -> (ctx, mempty)
  localize upd ma = Drawing $ \ctx -> getDrawing ma (upd ctx)



instance Monad m => DrawingCtxM (DrawingT u m) where
  askDC           = DrawingT $ \ctx -> return (ctx,mempty)
  localize upd ma = DrawingT $ \ctx -> getDrawingT ma (upd ctx)



-- Note - the result type of runDrawing and friends needs more 
-- thought and may change. 
--
-- Possibly a wrapped HPrim that only supports concat and safe
-- extraction is best.
--
-- Or it could generate a picture, but then separate drawings
-- need the picture combinators to put them together. 
-- 


runDrawing :: DrawingContext -> Drawing u a -> (a, HPrim u)
runDrawing ctx ma = getDrawing ma ctx

-- | Run the drawing returning only the output it produces, drop
-- any answer from the monadic computation.
--
execDrawing :: DrawingContext -> Drawing u a -> HPrim u
execDrawing ctx ma = snd $ runDrawing ctx ma

-- | Run the drawing ignoring the output it produces, return the 
-- answer from the monadic computation.
--
-- Note - this useful for testing, generally one would want the 
-- opposite behaviour (return the drawing, ignore than the 
-- answer).
-- 
evalDrawing :: DrawingContext -> Drawing u a -> a
evalDrawing ctx ma = fst $ runDrawing ctx ma



runDrawingT :: Monad m => DrawingContext -> DrawingT u m a -> m (a, HPrim u) 
runDrawingT ctx ma = getDrawingT ma ctx

execDrawingT :: Monad m => DrawingContext -> DrawingT u m a -> m (HPrim u)
execDrawingT ctx ma = liftM snd $ runDrawingT ctx ma


evalDrawingT :: Monad m => DrawingContext -> DrawingT u m a -> m a
evalDrawingT ctx ma = liftM fst $ runDrawingT ctx ma



-- | Run the Drawing generating a Picture /within/ a 
-- \"font delta context\" using the font-family and font-size 
-- from the intial DrawingContext.
--
-- Using a /font delta context/ can reduce the code size of the
-- generated SVG file (PostScript ignores the FDC).
--
runFdcDrawing :: (Real u, Floating u, FromPtSize u)
             => DrawingContext -> Drawing u a -> (a, Maybe (Picture u))
runFdcDrawing ctx ma = 
    let (a,hp) = runDrawing ctx ma
        ps     = hprimToList hp
        fdc    = font_props ctx
    in if null ps then (a, Nothing)
                 else (a, Just $ fontDeltaContext fdc $ frame ps)

-- | /exec/ version of 'runFdcContext'.
--
execFdcDrawing :: (Real u, Floating u, FromPtSize u)
             => DrawingContext -> Drawing u a -> Maybe (Picture u)
execFdcDrawing ctx ma = snd $ runFdcDrawing ctx ma

-- | Transformer version of 'runFdcDrawing'.
--
runFdcDrawingT :: (Real u, Floating u, FromPtSize u, Monad m)
             => DrawingContext -> DrawingT u m a -> m (a, Maybe (Picture u))
runFdcDrawingT ctx ma = 
    runDrawingT ctx ma >>= \(a,hp) -> 
    let ps     = hprimToList hp
        fdc    = font_props ctx
    in if null ps then return (a, Nothing)
                  else return (a, Just $ fontDeltaContext fdc $ frame ps)
  
-- | Transformer version of 'execFdcDrawing'.
--       
execFdcDrawingT :: (Real u, Floating u, FromPtSize u, Monad m)
             => DrawingContext -> DrawingT u m a -> m (Maybe (Picture u))
execFdcDrawingT ctx ma = liftM snd $ runFdcDrawingT ctx ma

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

query :: DrawingCtxM m => DrawingR a -> m a
query df = askDC >>= \ctx -> return $ runDrawingR ctx df


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


infixr 1 `at`
at :: DrawingR (Point2 u -> b) -> Point2 u -> DrawingR b
at = situ1




node :: (TraceM m, DrawingCtxM m, PointSupplyM m, u ~ MonUnit m) 
     => LocGraphic u -> m ()
node gf = askDC    >>= \ctx -> 
          position >>= \pt  -> 
          let f    = runDrawingR ctx gf in trace (collectH $ f pt)


nodei :: (TraceM m, DrawingCtxM m, PointSupplyM m, u ~ MonUnit m) 
     => LocImage u a -> m a
nodei imgL = askDC    >>= \ctx -> 
             position >>= \pt  -> 
             let (a,o) = runLocImage ctx imgL pt
             in trace (collectH o) >> return a

