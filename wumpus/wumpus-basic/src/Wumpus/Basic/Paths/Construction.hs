{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Paths.Construction
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Build paths.
--
-- \*\* WARNING \*\* this module is an experiment, and may 
-- change significantly or even be dropped from future revisions.
-- 
--------------------------------------------------------------------------------

module Wumpus.Basic.Paths.Construction
  ( 

    PathM
  , runPath
  , execPath
  , lineto
  , rlineto
  , vline
  , hline
  , bezierto
  , curveto
  , verticalHorizontal
  , horizontalVertical

  ) where

import Wumpus.Basic.Paths.Base

import Wumpus.Core                              -- package: wumpus-core

import Data.AffineSpace                         -- package: vector-space

import Control.Applicative

data PathState u = PathState 
      { current_point :: Point2 u 
      , path_accum    :: Path u
      }

newtype PathM u a = PathM { getPathM :: PathState u -> (a,PathState u) }


instance Functor (PathM u) where
  fmap f mf = PathM $ \s -> let (a,s1) = getPathM mf s in (f a,s1)


instance Applicative (PathM u) where
  pure a    = PathM $ \s -> (a,s)
  mf <*> ma = PathM $ \s -> let (f,s1) = getPathM mf s
                                (a,s2) = getPathM ma s1
                            in (f a,s2)

instance Monad (PathM u) where
  return a  = PathM $ \s -> (a,s)
  m >>= k   = PathM $ \s -> let (a,s1) = getPathM m s in
                            (getPathM . k) a s1


-- Design note - it is probably best to follow LRText and have
-- the path monad isolated from the trace monad. While it would 
-- be nice to trace arbitrary labels as we go, state changes to the 
-- DrawingCtx would make things complicated.
--

{-
openStrokePathM :: (Num u, TraceM m, DrawingCtxM m, u ~ MonUnit m) 
                => Point2 u -> PathM u a -> m a
openStrokePathM pt ma = let (a,p) = runPath pt ma  in 
    draw (openStroke $ toPrimPathU p) >> return a
-}

-- Running the path is agnostic to the DrawingCtx.

runPath :: Num u => Point2 u -> PathM u a -> (a, Path u)
runPath start mf = let (a,s') = getPathM mf s in (a, path_accum s')
  where
    s = PathState { current_point = start
                  , path_accum    = emptyPath
                  }

execPath :: Num u => Point2 u -> PathM u a -> Path u
execPath start mf = snd $ runPath start mf


exchTip :: Point2 u -> (Point2 u -> Path u -> Path u) -> PathM u ()
exchTip new updP = 
    PathM $ \(PathState old bp) -> ((), PathState new (updP old bp)) 

tip :: PathM u (Point2 u)
tip = PathM $ \s -> (current_point s,s)


lineto :: Floating u => Point2 u -> PathM u ()
lineto end = exchTip end upd
  where
    upd start bp = bp `addSegment` pline start end


rlineto :: Floating u => Vec2 u -> PathM u ()
rlineto (V2 dx dy) = tip >>= \(P2 x y) -> lineto (P2 (x+dx) (y+dy))

hline :: Floating u => u -> PathM u ()
hline dx = tip >>= \(P2 x y) -> lineto (P2 (x+dx) y)

vline :: Floating u => u -> PathM u ()
vline dy = tip >>= \(P2 x y) -> lineto (P2 x (y+dy))
 

bezierto :: (Floating u, Ord u) 
         => Point2 u -> Point2 u -> Point2 u -> PathM u ()
bezierto cp1 cp2 end = exchTip end upd 
  where
    upd start bp = bp `addSegment` pcurve start cp1 cp2 end


curveto :: (Floating u, Ord u) 
        => Radian -> Radian -> Point2 u -> PathM u ()
curveto cin cout end = exchTip end upd
  where 
    upd start bp = bp `addSegment` pcurveAng start cin cout end


pcurveAng :: (Floating u, Ord u) 
        => Point2 u -> Radian -> Radian -> Point2 u -> PathSeg u
pcurveAng start cin cout end = pcurve start (start .+^ v1) (end .+^ v2) end
  where
    sz     = 0.375 * (vlength $ pvec start end)
    v1     = avec cin  sz
    v2     = avec cout sz


verticalHorizontal :: Floating u => Point2 u -> PathM u ()
verticalHorizontal (P2 x y) = 
    tip >>= \(P2 x0 _) -> lineto (P2 x0 y) >> lineto (P2 x y)

horizontalVertical :: Floating u => Point2 u -> PathM u ()
horizontalVertical (P2 x y) = 
    tip >>= \(P2 _ y0) -> lineto (P2 x y0) >> lineto (P2 x y)