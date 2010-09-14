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
-- Drawing with trace and drawing context (i.e. reader monad
-- of attributes - fill_colour etc.).
--
--------------------------------------------------------------------------------

module Wumpus.Basic.Graphic.Drawing
  (

    Drawing
  , DrawingT
  , runDrawing
  , execDrawing
  , runDrawingT
  , execDrawingT
  , liftToPictureU
 
  , draw
  , drawAt
  , drawAtImg
  , drawConn
  , drawConnImg
  , node

  ) where


import Wumpus.Basic.Graphic.BaseClasses
import Wumpus.Basic.Graphic.BaseTypes
import Wumpus.Basic.Graphic.DrawingContext
import Wumpus.Basic.Utils.HList
 

import Wumpus.Core                              -- package: wumpus-core

import Control.Applicative
import Control.Monad




-- Note - Drawing run \once\ - it is supplied with the starting
-- environment (DrawingContext) and returns a Picture.
--
-- Other Wumpus monads (e.g. Turtle) will typically be run inside
-- the Drawing monad as a local effect, rather than built into a 
-- transformer stack.
--


newtype Drawing u a   = Drawing { 
          getDrawing :: DrawingContext -> HPrim u -> (a, HPrim u) }

newtype DrawingT u m a = DrawingT { 
          getDrawingT :: DrawingContext -> HPrim u -> m (a, HPrim u) }



type instance MonUnit (Drawing u) = u
type instance MonUnit (DrawingT u m) = u



-- Functor

instance Functor (Drawing u) where
  fmap f ma = Drawing $ \ctx s -> 
                let (a,s1) = getDrawing ma ctx s in (f a,s1)


instance Monad m => Functor (DrawingT u m) where
  fmap f ma = DrawingT $ \ctx s -> 
                getDrawingT ma ctx s >>= \(a,s1) -> return (f a,s1)



-- Applicative

instance Applicative (Drawing u) where
  pure a    = Drawing $ \_   s -> (a, s)
  mf <*> ma = Drawing $ \ctx s -> let (f,s1) = getDrawing mf ctx s
                                      (a,s2) = getDrawing ma ctx s1
                                 in (f a, s2)


instance Monad m => Applicative (DrawingT u m) where
  pure a    = DrawingT $ \_   s -> return (a, s)
  mf <*> ma = DrawingT $ \ctx s -> getDrawingT mf ctx s  >>= \(f,s1) ->
                                   getDrawingT ma ctx s1 >>= \(a,s2) ->
                                   return (f a, s2)

-- Monad

instance Monad (Drawing u) where
  return a  = Drawing $ \_   s -> (a, s)
  ma >>= k  = Drawing $ \ctx s -> let (a,s1) = getDrawing ma ctx s
                                  in (getDrawing . k) a ctx s1
                               



instance Monad m => Monad (DrawingT u m) where
  return a  = DrawingT $ \_   s -> return (a, s)
  ma >>= k  = DrawingT $ \ctx s -> getDrawingT ma ctx s       >>= \(a,s1) ->
                                   (getDrawingT . k) a ctx s1
                                 



-- TraceM 


instance TraceM (Drawing u) where
  trace a = Drawing $ \_ s -> ((),a `appendH` s)


instance Monad m => TraceM (DrawingT u m) where
  trace a = DrawingT $ \_ s -> return ((),a `appendH` s)



-- DrawingCtxM

instance DrawingCtxM (Drawing u) where
  askCtx         = Drawing $ \ctx s -> (ctx, s)
  localCtx cF ma = Drawing $ \ctx  s -> getDrawing ma (cF ctx) s



instance Monad m => DrawingCtxM (DrawingT u m) where
  askCtx         = DrawingT $ \ctx s -> return (ctx,s)
  localCtx cF ma = DrawingT $ \ctx s -> getDrawingT ma (cF ctx) s






runDrawing :: DrawingContext -> Drawing u a -> (a, HPrim u)
runDrawing ctx ma = getDrawing ma ctx emptyH

execDrawing :: DrawingContext -> Drawing u a -> HPrim u
execDrawing ctx ma = snd $ runDrawing ctx ma



runDrawingT :: Monad m => DrawingContext -> DrawingT u m a -> m (a, HPrim u) 
runDrawingT ctx ma = getDrawingT ma ctx emptyH

execDrawingT :: Monad m => DrawingContext -> DrawingT u m a -> m (HPrim u)
execDrawingT ctx ma = liftM snd $ runDrawingT ctx ma


liftToPictureU :: (Real u, Floating u, FromPtSize u) => HPrim u -> Picture u
liftToPictureU hf = let prims = toListH hf in 
                    if null prims then errK else frame prims
  where
    errK = error "liftToPictureU - empty prims list."

--------------------------------------------------------------------------------

draw :: (TraceM m, DrawingCtxM m, u ~ MonUnit m) => Graphic u -> m ()
draw gf = askCtx >>= \ctx -> trace (runGraphic ctx gf)

drawAt :: (TraceM m, DrawingCtxM m, u ~ MonUnit m) 
       => Point2 u ->LocGraphic u -> m ()
drawAt pt gfL = askCtx >>= \ctx -> trace (runGraphic ctx (gfL pt))

drawAtImg :: (TraceM m, DrawingCtxM m, u ~ MonUnit m) 
          => Point2 u -> LocImage u a -> m a
drawAtImg pt imgL = askCtx >>= \ctx -> 
                    let (a,o) = runImage ctx (imgL pt)
                    in trace o >> return a


     
drawConn :: (TraceM m, DrawingCtxM m, u ~ MonUnit m) 
         => Point2 u -> Point2 u -> ConnGraphic u -> m ()
drawConn p1 p2 connL = askCtx >>= \ctx -> trace (runGraphic ctx (connL p1 p2))
     
drawConnImg :: (TraceM m, DrawingCtxM m, u ~ MonUnit m) 
            => Point2 u -> Point2 u -> ConnImage u a -> m a
drawConnImg p1 p2 connL = askCtx >>= \ctx -> 
                          let (a,o) = runImage ctx (connL p1 p2)
                          in trace o >> return a
            


node :: (TraceM m, DrawingCtxM m, PointSupplyM m, u ~ MonUnit m) 
     => LocGraphic u -> m ()
node gfL = askCtx   >>= \ctx -> 
           position >>= \pt  -> trace (runGraphic ctx $ gfL pt)


