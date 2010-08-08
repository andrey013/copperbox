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
-- Drawing operations
--
--------------------------------------------------------------------------------

module Wumpus.Basic.Monads.Drawing
  (
    
    AGraphic(..)
  , AGraphic2(..)
  , node
  , at
  , connect
  , props

  ) where

import Wumpus.Basic.Graphic
import Wumpus.Basic.Graphic.DrawingAttr
import Wumpus.Basic.Monads.DrawingCtxClass
import Wumpus.Basic.Monads.TraceClass
import Wumpus.Basic.Monads.TurtleClass

import Wumpus.Core                              -- package: wumpus-core

import Control.Applicative

-- | AGraphic2 for /nodes/...
--
data AGraphic u a = AGraphic 
       { agAttrF    :: DrawingAttr -> DrawingAttr
       , agDrawF    :: DrawingAttr -> Point2 u -> Graphic u
       , agMakeF    :: DrawingAttr -> Point2 u -> a
       }

instance Functor (AGraphic u) where
  fmap f (AGraphic af df mf) = AGraphic af df (\attr pt -> f $ mf attr pt)

instance Applicative (AGraphic u) where
  pure a = AGraphic id (\_ _ -> id) (\_ _ -> a)
  (AGraphic af1 df1 mf1) <*> (AGraphic af2 df2 mf2) = AGraphic af df mf
      where
        af           = af2 . af1
        df attr pt   = df2 (af2 attr) pt . df1 (af1 attr) pt
        mf attr pt   = mf1 attr pt $ mf2 attr pt

-- | AGraphic2 for connectors...
--
data AGraphic2 u a = AGraphic2 
       { ag2AttrF    :: DrawingAttr -> DrawingAttr
       , ag2DrawF    :: DrawingAttr -> Point2 u -> Point2 u -> Graphic u
       , ag2MakeF    :: DrawingAttr -> Point2 u -> Point2 u -> a
       }

instance Functor (AGraphic2 u) where
  fmap f (AGraphic2 af df mf) = 
      AGraphic2 af df (\attr p1 p2 -> f $ mf attr p1 p2)


instance Applicative (AGraphic2 u) where
  pure a = AGraphic2 id (\_ _ _ -> id) (\_ _ _ -> a)
  (AGraphic2 af1 df1 mf1) <*> (AGraphic2 af2 df2 mf2) = AGraphic2 af df mf
      where
        af            = af2 . af1 
        df attr p1 p2 = df2 (af2 attr) p1 p2 . df1 (af1 attr) p1 p2
        mf attr p1 p2 = mf1 attr p1 p2 $ mf2 attr p1 p2



-- This doesn't work like at on MGraphicF, as the point is not 
-- scaled w.r.t. TurtleScaleM ...
--
at :: AGraphic u a -> Point2 u -> AGraphic u a
at (AGraphic af df mf) pt = AGraphic af (\attr _ -> df attr pt)
                                        (\attr _ -> mf attr pt)



-- getPos should be a class method outside of Turtle
-- those Bivariate context from PSC could implement it...

node :: (Num u, TraceM m (Primitive u), DrawingCtxM m, TurtleScaleM m u) 
       => AGraphic u a -> m a
node (AGraphic af df mf) = 
    askDrawingCtx >>= \a0 ->
    getPos        >>= \pt ->
    let attr = af a0 in trace (df attr pt) >> return (mf attr pt)



connect :: (Num u, TraceM m (Primitive u), DrawingCtxM m) 
        => AGraphic2 u a -> Point2 u -> Point2 u -> m a
connect (AGraphic2 af df mf) p1 p2 = 
    askDrawingCtx >>= \a0 ->
    let attr = af a0 in trace (df attr p1 p2) >> return (mf attr p1 p2)

infixr 7 `props`

props :: AGraphic u a -> (DrawingAttr -> DrawingAttr) -> AGraphic u a
props (AGraphic attrF drawF mkF) updF = AGraphic (updF . attrF) drawF mkF
