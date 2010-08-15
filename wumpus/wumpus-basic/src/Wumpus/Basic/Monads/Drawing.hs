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
--------------------------------------------------------------------------------

module Wumpus.Basic.Monads.Drawing
  (
    
    AGraphic(..)
  , AGraphic2(..)
  , node
  , at
  , liftAG
  , liftAG2
  , connect
  , props

  -- doodle
  , thick
  , thick2

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

node :: (Num u, TraceM m u, DrawingCtxM m, TurtleScaleM m u) 
       => AGraphic u a -> m a
node (AGraphic af df mf) = 
    askDrawingCtx >>= \a0 ->
    getPos        >>= \pt ->
    let attr = af a0 in trace (df attr pt) >> return (mf attr pt)


liftAG :: (Num u, TraceM m u, DrawingCtxM m) 
       => AGraphic u a -> Point2 u -> m a
liftAG (AGraphic af df mf) pt = 
    askDrawingCtx >>= \a0 ->
    let attr = af a0 in trace (df attr pt) >> return (mf attr pt)


liftAG2 :: (Num u, TraceM m u, DrawingCtxM m) 
       => AGraphic2 u a -> Point2 u -> Point2 u -> m a
liftAG2 (AGraphic2 af df mf) p1 p2 = 
    askDrawingCtx >>= \a0 ->
    let attr = af a0 in trace (df attr p1 p2) >> return (mf attr p1 p2)




connect :: (Num u, TraceM m u, DrawingCtxM m) 
        => AGraphic2 u a -> Point2 u -> Point2 u -> m a
connect (AGraphic2 af df mf) p1 p2 = 
    askDrawingCtx >>= \a0 ->
    let attr = af a0 in trace (df attr p1 p2) >> return (mf attr p1 p2)




infixr 7 `props`

props :: AGraphic u a -> (DrawingAttr -> DrawingAttr) -> AGraphic u a
props (AGraphic attrF drawF mkF) updF = AGraphic (updF . attrF) drawF mkF


-- Just a doodle at the moment 
-- 
-- Property changing could be composable...
--
-- Really ought to be (*2) as well, but for the moment (*4) is
-- easier to see in the ouput. 
--

thick :: AGraphic u a -> AGraphic u a 
thick (AGraphic af df mf) = AGraphic (upd . af) df mf
  where
    upd = star (\s i -> s { line_width = i*4 }) line_width

-- Clearly having thick2 is bad...
--
thick2 :: AGraphic2 u a -> AGraphic2 u a 
thick2 (AGraphic2 af df mf) = AGraphic2 (upd . af) df mf
  where
    upd = star (\s i -> s { line_width = i*4 }) line_width


star     :: (r -> a -> ans) 
         -> (r -> a) 
         -> r -> ans
star f fa x = f x (fa x)
