{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Kernel.Objects.TraceDrawing
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
 
--  , convertTraceDrawing
--  , convertTraceDrawingT

  , runQuery

  , draw
  , drawi
  , drawi_
  , drawl
  , drawli
  , drawli_
  , drawc
  , drawci
  , drawci_


  , xdraw
  , xdrawi
  , xdrawi_

  , node
  , nodei
  , nodei_

  , cxdraw
  , cxdrawi
  , cxdrawi_
 
  , drawrc
  , drawrci
  , drawrci_ 

  ) where


import Wumpus.Basic.Kernel.Base.Anchors
import Wumpus.Basic.Kernel.Base.BaseDefs
import Wumpus.Basic.Kernel.Base.ContextFun
import Wumpus.Basic.Kernel.Base.DrawingContext
import Wumpus.Basic.Kernel.Base.QueryDC
import Wumpus.Basic.Kernel.Base.WrappedPrimitive
import Wumpus.Basic.Kernel.Objects.BaseObjects
import Wumpus.Basic.Kernel.Objects.Connector
import Wumpus.Basic.Kernel.Objects.Graphic
import Wumpus.Basic.Kernel.Objects.UnitConvert

import Wumpus.Core                              -- package: wumpus-core

import Control.Applicative
import Control.Monad
import Data.Monoid


--------------------------------------------------------------------------------

-- | Collect elementary graphics as part of a larger drawing.
--
-- TraceM works much like a writer monad.
--
class TraceM m u | m -> u where
  trace  :: HPrim u -> m ()


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

instance TraceM (TraceDrawing u) u where
  trace a = TraceDrawing $ \_ -> ((), a)


instance Monad m => TraceM (TraceDrawingT u m) u where
  trace a = TraceDrawingT $ \_ -> return ((), a)



-- DrawingCtxM

instance DrawingCtxM (TraceDrawing u) where
  queryCtx        = TraceDrawing $ \ctx -> (ctx, mempty)
  localize upd ma = TraceDrawing $ \ctx -> getTraceDrawing ma (upd ctx)



instance Monad m => DrawingCtxM (TraceDrawingT u m) where
  queryCtx        = TraceDrawingT $ \ctx -> return (ctx,mempty)
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



{-
-- Conversion is up in the air...
convertTraceDrawing :: ConvertAlg a u1 b u 
                    -> TraceDrawing u1 a -> TraceDrawing u b
convertTraceDrawing (ConvertAlg _ f2 f3) mf = TraceDrawing $ \ctx -> 
    bimap f3 (fmap f2) $ getTraceDrawing mf ctx


convertTraceDrawingT :: Monad m 
                     => ConvertAlg a u1 b u
                     -> TraceDrawingT u1 m a -> TraceDrawingT u m b
convertTraceDrawingT (ConvertAlg _ f2 f3) mf = TraceDrawingT $ \ctx -> 
    liftM (bimap f3 (fmap f2)) $ getTraceDrawingT mf ctx
-}

--------------------------------------------------------------------------------

runQuery :: DrawingCtxM m => DrawingInfo a -> m a
runQuery df = queryCtx >>= \ctx -> return $ runCF ctx df



-- | Draw a Graphic taking the drawing style from the 
-- /drawing context/. 
--
-- This operation is analogeous to @tell@ in a Writer monad.
-- 
draw :: (TraceM m u, DrawingCtxM m) 
     => Graphic u -> m ()
draw gf = queryCtx >>= \ctx -> 
          trace (singleH $ snd $ getImageAns $ runCF ctx gf)



-- | Draw an Image taking the drawing style from the 
-- /drawing context/. 
--
-- The graphic representation of the Image is drawn in the Trace 
-- monad, and the result is returned.
-- 
drawi :: (TraceM m u, DrawingCtxM m) 
      => Image u a -> m a
drawi img = queryCtx >>= \ctx -> 
            let (a,o) = getImageAns $ runCF ctx img 
            in trace (singleH o) >> return a


-- | Forgetful 'drawi'.
--
drawi_ :: (TraceM m u, DrawingCtxM m) 
       => Image u a -> m ()
drawi_ img = drawi img >> return ()


-- | Draw a LocGraphic at the supplied Point taking the drawing 
-- style from the /drawing context/. 
--
-- This operation is analogeous to @tell@ in a Writer monad.
-- 
drawl :: (TraceM m u, DrawingCtxM m) 
      => Point2 u -> LocGraphic u -> m ()
drawl pt gf = queryCtx >>= \ctx -> 
              trace (singleH $ snd $ getImageAns $ runCF ctx $ gf `at` pt)



-- | Draw a LocImage at the supplied Point taking the drawing 
-- style from the /drawing context/. 
--
-- The graphic representation of the Image is drawn in the Trace 
-- monad, and the result is returned.
-- 
drawli :: (TraceM m u, DrawingCtxM m) 
       => Point2 u -> LocImage u a -> m a
drawli pt img = drawi (img `at` pt)

-- | Forgetful 'drawli'.
--
drawli_ :: (TraceM m u, DrawingCtxM m) 
        => Point2 u -> LocImage u a -> m ()
drawli_ pt img = drawli pt img >> return ()


-- Design note - having @drawlti@ for LocThetaImage does not seem 
-- compelling (at the moment). The thinking is that LocTheta
-- objects should be downcast to Loc objects before drawing. 
--
-- Connectors however are be different. 
-- 
-- (PosImages are still to consider).
--



-- | Draw a ConnectorGraphic with the supplied Points taking the 
-- drawing style from the /drawing context/. 
--
drawc :: (TraceM m u, DrawingCtxM m) 
      => Point2 u -> Point2 u -> ConnectorGraphic u -> m ()
drawc p0 p1 gf = draw (connect gf p0 p1)

-- | Draw a ConnectorImage with the supplied Points taking the 
-- drawing style from the /drawing context/. 
--
-- The graphic representation of the Image is drawn in the Trace 
-- monad, and the result is returned.
-- 
drawci :: (TraceM m u, DrawingCtxM m) 
       => Point2 u -> Point2 u -> ConnectorImage u a -> m a
drawci p0 p1 img = drawi (connect img p0 p1)


-- | Forgetful 'drawci'.
--
drawci_ :: (TraceM m u, DrawingCtxM m) 
        => Point2 u -> Point2 u -> ConnectorImage u a -> m ()
drawci_ p0 p1 img = drawi (connect img p0 p1) >> return ()



-- Note - hyperlinking should probably use 'hyperlink' to annotate
-- Objects before drawing them. Having hyperlink versions of the 
-- trace drawing functions seems like a mistake and leads to API 
-- clutter.


-- | Hyperlink version of 'draw'.
--
-- Note - hyperlinking should probably use 'hyperlink' to annotate
-- Objects before drawing them. Having hyperlink versions of the 
-- trace drawing functions seems like a mistake and leads to API 
-- clutter, so this function is considered obsolete.
--
xdraw :: (TraceM m u, DrawingCtxM m) 
      => XLink -> Graphic u -> m ()
xdraw xl gf = draw (hyperlink xl gf)



-- | Hyperlink version of 'drawi'.
--
-- Note - hyperlinking should probably use 'hyperlink' to annotate
-- Objects before drawing them. Having hyperlink versions of the 
-- trace drawing functions seems like a mistake and leads to API 
-- clutter, so this function is considered obsolete.
--
xdrawi ::  (TraceM m u, DrawingCtxM m) 
       => XLink -> Image u a -> m a
xdrawi xl img = drawi (hyperlink xl img)


-- | Forgetful 'xdrawi'.
--
-- Note - hyperlinking should probably use 'hyperlink' to annotate
-- Objects before drawing them. Having hyperlink versions of the 
-- drawing functions seems like a mistake and leads to API 
-- clutter, so this function is considered obsolete.
--
xdrawi_ ::  (TraceM m u, DrawingCtxM m)
        => XLink -> Image u a -> m ()
xdrawi_ xl img = xdrawi xl img >> return ()




-- | Draw with grid coordinate...
--
node :: (Fractional u, PtSize u, TraceM m u, DrawingCtxM m) 
     => (Int,Int) -> LocGraphic u -> m ()
node coord gf = queryCtx       >>= \ctx -> 
                position coord >>= \pt  -> 
                let (_,prim) = getImageAns $ runCF1 ctx pt gf 
                in trace (singleH prim)


-- | Draw with grid coordinate...
-- 
nodei :: ( Fractional u, PtSize u, TraceM m u, DrawingCtxM m)
      => (Int,Int) -> LocImage u a -> m a
nodei coord imgL = queryCtx    >>= \ctx -> 
                   position coord >>= \pt  -> 
                   let (a,o) = getImageAns $ runCF ctx (apply1R1 imgL pt)
                   in trace (singleH o) >> return a



 
-- | Draw with grid coordinate...
--
nodei_ :: (Fractional u, TraceM m u, PtSize u, DrawingCtxM m)
       => (Int,Int) -> LocImage u a -> m ()
nodei_ coord imgL = nodei coord imgL >> return ()



cxdraw :: (Fractional u, TraceM m u, DrawingCtxM m) 
       => DrawingInfo (Point2 u) -> LocGraphic u -> m ()
cxdraw pf gf = 
    queryCtx  >>= \ctx -> let pt    = runCF  ctx pf
                              (_,o) = getImageAns $ runCF1 ctx pt gf 
                          in trace (singleH o)

cxdrawi :: (Fractional u, TraceM m u, DrawingCtxM m) 
       => DrawingInfo (Point2 u) -> LocImage u a -> m a
cxdrawi pf gf =  
    queryCtx  >>= \ctx -> let pt    = runCF  ctx pf
                              (a,o) = getImageAns $ runCF1 ctx pt gf 
                          in trace (singleH o) >> return a

cxdrawi_ :: (Fractional u, TraceM m u, DrawingCtxM m) 
        => DrawingInfo (Point2 u) -> LocImage u a -> m ()
cxdrawi_ pf gf = cxdrawi pf gf >> return ()



drawrc :: ( Real u, Floating u, PtSize u
          , CenterAnchor t1 u, RadialAnchor  t1 u
          , CenterAnchor t2 u, RadialAnchor  t2 u
          , DrawingCtxM m,   TraceM m u
          ) 
       => t1 -> t2 -> ConnectorGraphic  u -> m ()
drawrc a b gf = let (p0,p1) = radialConnectorPoints a b 
                in draw (connect gf p0 p1)


drawrci :: ( Real u, Floating u, PtSize u
           , CenterAnchor t1 u, RadialAnchor  t1 u
           , CenterAnchor t2 u, RadialAnchor  t2 u
           , DrawingCtxM m,   TraceM m u
           ) 
        => t1 -> t2 -> ConnectorImage u a -> m a
drawrci a b gf = let (p0,p1) = radialConnectorPoints a b 
                 in drawci p0 p1 gf

drawrci_ :: ( Real u, Floating u, PtSize u
            , CenterAnchor t1 u, RadialAnchor  t1 u
            , CenterAnchor t2 u, RadialAnchor  t2 u
            , DrawingCtxM m,   TraceM m u
            ) 
         => t1 -> t2 -> ConnectorImage u a -> m ()
drawrci_ a b gf = drawrci a b gf >> return ()
