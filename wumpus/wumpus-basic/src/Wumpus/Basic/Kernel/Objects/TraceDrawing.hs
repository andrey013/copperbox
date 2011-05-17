{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Kernel.Objects.TraceDrawing
-- Copyright   :  (c) Stephen Tetley 2010-2011
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

module Wumpus.Basic.Kernel.Objects.TraceDrawing
  (

  -- * Collect primitives (writer monad) 
    TraceM(..)

  , TraceDrawing
  , DTraceDrawing
  , TraceDrawingT
  , DTraceDrawingT

  , runTraceDrawing
  , execTraceDrawing
  , evalTraceDrawing
  , runTraceDrawingT
  , execTraceDrawingT
  , evalTraceDrawingT

  , liftToPictureU
  , liftToPictureMb
  , mbPictureU
 

  , evalQuery

  , draw
  , drawi
  , drawl
  , drawli

  , drawc
  , drawci

  , node
  , nodei
 
  , drawrc
  , drawrci

  ) where


import Wumpus.Basic.Kernel.Base.BaseDefs
import Wumpus.Basic.Kernel.Base.DrawingContext
import Wumpus.Basic.Kernel.Base.QueryDC
import Wumpus.Basic.Kernel.Base.WrappedPrimitive
import Wumpus.Basic.Kernel.Objects.Anchors
import Wumpus.Basic.Kernel.Objects.Basis
import Wumpus.Basic.Kernel.Objects.Connector
import Wumpus.Basic.Kernel.Objects.LocImage

import Wumpus.Core                              -- package: wumpus-core

import Control.Applicative
import Control.Monad
import Data.Monoid


--------------------------------------------------------------------------------



-- | Collect elementary graphics as part of a larger drawing.
--
-- TraceM works much like a writer monad.
--
class TraceM (m :: * -> *) where
  trace     :: MonUnit (m ()) ~ u => HPrim u -> m ()
  fontDelta :: m a -> m a

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

type instance MonUnit (TraceDrawing u a) = u
type instance MonUnit (TraceDrawingT u m a) = u


type DTraceDrawing a    = TraceDrawing Double a
type DTraceDrawingT m a = TraceDrawingT Double m a



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
  fontDelta = fontDeltaMon

fontDeltaMon :: TraceDrawing u a -> TraceDrawing u a
fontDeltaMon mf = TraceDrawing $ \ctx -> 
    let (_,font_attrs) = primAnswer $ runImage ctx textAttr
        (a,hf)         = runTraceDrawing ctx mf
        prim           = fontDeltaContext font_attrs $ primGroup $ hprimToList hf
    in (a, singleH $ prim1 $ prim)

instance Monad m => TraceM (TraceDrawingT u m) where
  trace a = TraceDrawingT $ \_ -> return ((), a)
  fontDelta = fontDeltaTrans

fontDeltaTrans :: Monad m => TraceDrawingT u m a -> TraceDrawingT u m a
fontDeltaTrans mf = TraceDrawingT $ \ctx -> 
    let (_,font_props) = primAnswer $ runImage ctx textAttr
    in runTraceDrawingT ctx mf >>= \(a,hf) ->
       let prim  = fontDeltaContext font_props $ primGroup $ hprimToList hf
       in return (a, singleH $ prim1 $ prim)



-- DrawingCtxM

instance DrawingCtxM (TraceDrawing u) where
  askDC           = TraceDrawing $ \ctx -> (ctx, mempty)
  asksDC f        = TraceDrawing $ \ctx -> (f ctx, mempty)
  localize upd ma = TraceDrawing $ \ctx -> getTraceDrawing ma (upd ctx)



instance Monad m => DrawingCtxM (TraceDrawingT u m) where
  askDC           = TraceDrawingT $ \ctx -> return (ctx, mempty)
  asksDC f        = TraceDrawingT $ \ctx -> return (f ctx, mempty)
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



-- | /Unsafe/ promotion of @HPrim@ to @Picture@.
--
-- If the HPrim is empty, a run-time error is thrown.
-- 
liftToPictureU :: HPrim u -> Picture
liftToPictureU hf = 
    let prims = hprimToList hf in if null prims then errK else frame prims
  where
    errK = error "toPictureU - empty prims list."

-- | /Safe/ promotion of @HPrim@ to @(Maybe Picture)@.
--
-- If the HPrim is empty, then @Nothing@ is returned.
-- 
liftToPictureMb :: HPrim u -> Maybe Picture
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
mbPictureU :: Maybe Picture -> Picture
mbPictureU Nothing  = error "mbPictureU - empty picture."
mbPictureU (Just a) = a

-- Note - need an equivalent to Parsec\`s parseTest that provides
-- a very simple way to run graphics without concern for return 
-- type or initial drawing context.



--------------------------------------------------------------------------------


evalQuery :: DrawingCtxM m => Query u a -> m a
evalQuery df = askDC >>= \ctx -> return $ runQuery ctx df



-- | Draw a Graphic taking the drawing style from the 
-- /drawing context/. 
--
-- This function is the /forgetful/ version of 'drawi'. 
-- Commonly, it is used to draw 'Graphic' objects which 
-- have no /answer/.
-- 
draw :: (TraceM m, DrawingCtxM m, u ~ MonUnit (m ()) ) 
     => Image u a -> m ()
draw gf = askDC >>= \ctx -> 
          let (PrimW o _) = runImage ctx gf
          in trace (singleH o) >> return ()




-- | Draw an Image taking the drawing style from the 
-- /drawing context/. 
--
-- The graphic representation of the Image is drawn in the Trace 
-- monad, and the result is returned.
-- 
drawi :: (TraceM m, DrawingCtxM m, u ~ MonUnit (m ()) ) 
      => Image u a -> m a
drawi gf = askDC >>= \ctx -> 
           let (PrimW o a) = runImage ctx gf 
           in trace (singleH o) >> return a
            


-- | Draw a LocImage at the supplied Anchor taking the drawing 
-- style from the /drawing context/. 
--
-- This function is the /forgetful/ version of 'drawli'. 
-- Commonly, it is used to draw 'LocGraphic' objects which 
-- have no /answer/.
-- 
drawl :: (TraceM m, DrawingCtxM m, u ~ MonUnit (m ()) ) 
      => Anchor u -> LocImage u a -> m ()
drawl ancr img = drawli ancr img >> return ()



-- | Draw a LocImage at the supplied Point taking the drawing 
-- style from the /drawing context/. 
--
-- The graphic representation of the Image is drawn in the Trace 
-- monad, and the result is returned.
-- 
drawli :: (TraceM m, DrawingCtxM m, u ~ MonUnit (m ()) ) 
       => Anchor u -> LocImage u a -> m a
drawli pt gf = askDC >>= \ctx -> 
               let (PrimW o a) = runLocImage pt ctx gf
               in trace (singleH o) >> return a


-- Design note - having @drawlti@ for LocThetaImage does not seem 
-- compelling (at the moment). The thinking is that LocTheta
-- objects should be downcast to Loc objects before drawing. 
--
-- Connectors however are be different. 
-- 
-- PosImages would seem to be the same as LocThetaImages.
--



-- | Draw a ConnectorGraphic with the supplied Anchors taking the 
-- drawing style from the /drawing context/. 
--
-- This function is the /forgetful/ version of 'drawci'. 
-- Commonly, it is used to draw 'ConnectorGraphic' objects which 
-- have no /answer/.
-- 
drawc :: (TraceM m, DrawingCtxM m, u ~ MonUnit (m ()) ) 
      => Anchor u -> Anchor u -> ConnectorImage u a -> m ()
drawc an0 an1 img = drawci an0 an1 img >> return () 


-- | Draw a ConnectorImage with the supplied Points taking the 
-- drawing style from the /drawing context/. 
--
-- The graphic representation of the Image is drawn in the Trace 
-- monad, and the result is returned.
-- 
drawci :: (TraceM m, DrawingCtxM m, u ~ MonUnit (m ()) ) 
       => Anchor u -> Anchor u -> ConnectorImage u a -> m a
drawci p0 p1 img = drawi (connect p0 p1 img)








-- | Draw the object with the supplied grid coordinate. The 
-- actual position is scaled according to the 
-- @snap_grid_factors@ in the /drawing context/.
-- 
-- This function is the /forgetful/ version of 'nodei'. 
-- Commonly, it is used to draw 'LocGraphic' objects which 
-- have no /answer/.
-- 
node :: (Fractional u, TraceM m, DrawingCtxM m, u ~ MonUnit (m ()) ) 
     => (Int,Int) -> LocImage u a -> m ()
node coord gf = nodei coord gf >> return ()


-- | Draw the object with the supplied grid coordinate. The 
-- actual position is scaled according to the 
-- @snap_grid_factors@ in the /drawing context/.
-- 
nodei :: (Fractional u, TraceM m, DrawingCtxM m, u ~ MonUnit (m ()) ) 
      => (Int,Int) -> LocImage u a -> m a
nodei coord gf = askDC >>= \ctx -> 
                 position coord >>= \pt ->
                 let (PrimW o a) = runLocImage pt ctx gf
                 in trace (singleH o) >> return a
 




-- | Draw a connector between two objects. The projection of the
-- connector line is drawn on the line from center to center of 
-- the objects, the actual start and end points of the drawn line
-- are the radial points on the objects borders that cross the 
-- projected line.
-- 
-- This function is the /forgetful/ version of 'drawrci'. 
-- Commonly, it is used to draw 'LocGraphic' objects which 
-- have no /answer/.
-- 
drawrc :: ( Real u, Floating u, DrawingCtxM m, TraceM m 
          , CenterAnchor a, RadialAnchor a
          , CenterAnchor b, RadialAnchor b
          , u ~ MonUnit (m ()), u ~ DUnit a, u ~ DUnit b 
          ) 
       => a -> b -> ConnectorImage u ans -> m ()
drawrc a b gf = drawrci a b gf >> return ()


-- | Draw a connector between two objects. The projection of the
-- connector line is drawn on the line from center to center of 
-- the objects, the actual start and end points of the drawn line
-- are the radial points on the objects borders that cross the 
-- projected line.
-- 
drawrci :: ( Real u, Floating u, DrawingCtxM m, TraceM m
           , CenterAnchor a, RadialAnchor  a
           , CenterAnchor b, RadialAnchor  b
           , u ~ MonUnit (m ()), u ~ DUnit a, u ~ DUnit b
           ) 
        => a -> b -> ConnectorImage u ans -> m ans
drawrci a b gf = 
    let (p0,p1) = radialConnectorPoints a b in drawi (connect p0 p1 gf)

