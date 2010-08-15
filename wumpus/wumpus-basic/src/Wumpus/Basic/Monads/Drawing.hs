{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Monads.Drawing
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC 
--
-- Drawing types and lifters...
--
-- Note - there are problems with this module. AGraphic2 does not
-- model connectors well.
--
-- For drawings it turns out that a connector isn\'t really a
-- function from two points to a Graphic, because drawing modes
-- never supply two points (e.g. turtle mode only ever supplies
-- one point, the current turtle position).
--
-- Connectors actually derive start and end points from objects - 
-- anchors on dots, shapes - rather than derive points from the 
-- drawing mode. So really they are arity 0 rather arity 2.
--
--
--------------------------------------------------------------------------------

module Wumpus.Basic.Monads.Drawing
  (
    
    AGraphic(..)
  , ANode
  , AFreeGraphic
  , AConnector

  , node
  , at
  , liftAFG
  , connect
  , connect_
  , props

  -- doodle
  , thick

  ) where

import Wumpus.Basic.Graphic
import Wumpus.Basic.Graphic.DrawingAttr
import Wumpus.Basic.Monads.DrawingCtxClass
import Wumpus.Basic.Monads.TraceClass
import Wumpus.Basic.Monads.TurtleClass

import Wumpus.Core                              -- package: wumpus-core

import Control.Applicative

-- | AGraphic 
-- 
-- param typically @Point2 u@ or @()@
--
-- If the param is a point it will be supplied by the drawing 
-- mode / drawing monad (e.g. the Turtle monad which supplies
-- the current point).
--
data AGraphic param u a = AGraphic 
       { agDrawF    :: DrawingAttr -> param -> Graphic u
       , agMakeF    :: DrawingAttr -> param -> a
       }


-- | ANode is drawn a a point supplied by the drawing 
-- (e.g. current node of Turtle).
--
type ANode u a = AGraphic (Point2 u) u a

-- /Free/ graphic
--
type AFreeGraphic u a = AGraphic () u a

type AConnector u a = Point2 u -> Point2 u -> AFreeGraphic u a

instance Functor (AGraphic pm u) where
  fmap f (AGraphic df mf) = AGraphic df (\pt attr -> f $ mf pt attr)

instance Applicative (AGraphic pm u) where
  pure a = AGraphic (\_ _ -> id) (\_ _ -> a)
  (AGraphic df1 mf1) <*> (AGraphic df2 mf2) = AGraphic df mf
      where
        df attr pt   = df2 attr pt . df1 attr pt
        mf attr pt   = mf1 attr pt $ mf2 attr pt


-- This doesn't work like at on MGraphicF, as the point is not 
-- scaled w.r.t. TurtleScaleM ...
--
at :: ANode u a -> Point2 u -> ANode u a
at (AGraphic df mf) pt = AGraphic (\attr _ -> df attr pt)
                                  (\attr _ -> mf attr pt)



-- getPos should be a class method outside of Turtle
-- those Bivariate context from PSC could implement it...

node :: (Num u, TraceM m u, DrawingCtxM m, TurtleScaleM m u) 
       => ANode u a -> m a
node (AGraphic df mf) = 
    askDrawingCtx >>= \attr ->
    getPos        >>= \pt   ->
    trace (df attr pt) >> return (mf attr pt)



liftAFG :: (Num u, TraceM m u, DrawingCtxM m) 
        => AFreeGraphic u a -> m a
liftAFG (AGraphic df mf) = 
    askDrawingCtx >>= \attr -> trace (df attr ()) >> return (mf attr ())



connect :: (Num u, TraceM m u, DrawingCtxM m) 
        => AConnector u a -> Point2 u -> Point2 u -> m a
connect conn p1 p2 = let (AGraphic df mf) = conn p1 p2 in  
    askDrawingCtx >>= \attr -> trace (df attr ()) >> return (mf attr ())


-- This is a bit unfortunate - with a connector we can\'t touch
-- the drawingAttr inside the AGraphic becase a connecter  is
--
-- > pt -> pt -> AGraphic
--
-- and not
--
-- > AGraphic
--
--
-- Maybe AGraphic shouldn\'t have the agAttrF field?
--
--
--
connect_ :: (Num u, TraceM m u, DrawingCtxM m) 
         => (DrawingAttr -> DrawingAttr) 
         -> AConnector u a -> Point2 u -> Point2 u -> m a
connect_ fn conn p1 p2 = let (AGraphic df mf) = conn p1 p2 in  
    askDrawingCtx >>= \a0 ->
    let attr = fn $ a0 in trace (df attr ()) >> return (mf attr ())




infixr 7 `props`

props :: AGraphic pm u a -> (DrawingAttr -> DrawingAttr) -> AGraphic pm u a
props (AGraphic df mf) upd = AGraphic (\attr p -> df (upd attr) p) 
                                      (\attr p -> mf (upd attr) p)



-- Just a doodle at the moment 
-- 
-- Property changing could be composable...
--
-- Really ought to be (*2) as well, but for the moment (*4) is
-- easier to see in the ouput. 
--

thick :: DrawingAttr -> DrawingAttr 
thick = star (\s i -> s { line_width = i*4 }) line_width


star     :: (r -> a -> ans) 
         -> (r -> a) 
         -> r -> ans
star f fa x = f x (fa x)
