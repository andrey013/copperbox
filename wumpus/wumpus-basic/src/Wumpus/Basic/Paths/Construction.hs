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

    MPath
  , CPath
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

newtype MPath u a = MPath { getMPath :: PathState u -> (a,PathState u) }

type CPath u = MPath u ()

instance Functor (MPath u) where
  fmap f mf = MPath $ \s -> let (a,s') = getMPath mf s in (f a,s')


instance Applicative (MPath u) where
  pure a    = MPath $ \s -> (a,s)
  mf <*> ma = MPath $ \s -> let (f,s')  = getMPath mf s
                                (a,s'') = getMPath ma s'
                            in (f a,s'')

instance Monad (MPath u) where
  return a  = MPath $ \s -> (a,s)
  m >>= k   = MPath $ \s -> let (a,s') = getMPath m s in
                            (getMPath . k) a s'


runPath :: Num u => Point2 u -> MPath u a -> (a, Path u)
runPath start mf = let (a,s') = getMPath mf s in (a, path_accum s')
  where
    s = PathState { current_point = start
                  , path_accum    = emptyPath
                  }

execPath :: Num u => Point2 u -> MPath u a -> Path u
execPath start mf = snd $ runPath start mf


exchTip :: Point2 u -> (Point2 u -> Path u -> Path u) -> MPath u ()
exchTip new updP = 
    MPath $ \(PathState old bp) -> ((), PathState new (updP old bp)) 

tip :: MPath u (Point2 u)
tip = MPath $ \s -> (current_point s,s)


lineto :: Floating u => Point2 u -> CPath u
lineto end = exchTip end upd
  where
    upd start bp = bp `addSegment` pline start end


rlineto :: Floating u => Vec2 u -> CPath u
rlineto (V2 dx dy) = tip >>= \(P2 x y) -> lineto (P2 (x+dx) (y+dy))

hline :: Floating u => u -> CPath u
hline dx = tip >>= \(P2 x y) -> lineto (P2 (x+dx) y)

vline :: Floating u => u -> CPath u
vline dy = tip >>= \(P2 x y) -> lineto (P2 x (y+dy))
 

bezierto :: (Floating u, Ord u) 
         => Point2 u -> Point2 u -> Point2 u -> CPath u
bezierto cp1 cp2 end = exchTip end upd 
  where
    upd start bp = bp `addSegment` pcurve start cp1 cp2 end


curveto :: (Floating u, Ord u) 
        => Radian -> Radian -> Point2 u -> CPath u
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


verticalHorizontal :: Floating u => Point2 u -> CPath u
verticalHorizontal (P2 x y) = 
    tip >>= \(P2 x0 _) -> lineto (P2 x0 y) >> lineto (P2 x y)

horizontalVertical :: Floating u => Point2 u -> CPath u
horizontalVertical (P2 x y) = 
    tip >>= \(P2 _ y0) -> lineto (P2 x y0) >> lineto (P2 x y)