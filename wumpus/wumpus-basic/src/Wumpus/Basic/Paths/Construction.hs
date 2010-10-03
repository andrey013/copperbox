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
-- Build paths monadically.
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

  , tip

  , lineto
  , rlineto
  , hline
  , vline

  , bezierto
  , curveto

  , verticalHorizontal
  , horizontalVertical

  ) where

import Wumpus.Basic.Paths.Base

import Wumpus.Core                              -- package: wumpus-core

import Data.AffineSpace                         -- package: vector-space

import Control.Applicative
import Data.Maybe
import Data.Monoid


-- Are connectors and paths quite different things?
--
-- It looks like they are - connectors always know start and end 
-- points.
--


-- State monad version is quite good - it ameliorates the problem
-- of joing to the end point of an empty path...

data St u = St
      { current_point :: Point2 u 
      , path_acc      :: Path u
      }


newtype PathM u a = PathM { getPathM :: St u -> (a,St u) }


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



-- Running the path is (probably) agnostic to the DrawingCtx.
--
runPath :: Floating u => Point2 u -> PathM u a -> (a, Path u)
runPath start mf = let (a,s') = getPathM mf s in (a, path_acc s')
  where
    s = St { current_point = start
           , path_acc      = mempty
           }

execPath :: Floating u => Point2 u -> PathM u a -> Path u
execPath start mf = snd $ runPath start mf

snocline :: Floating u => Vec2 u -> PathM u ()
snocline v = PathM $ \(St pt ac) -> let ep = pt .+^ v 
                                    in ((), St ep (ac `mappend` line pt ep))


tip :: PathM u (Point2 u)
tip = PathM $ \s -> (current_point s,s)


lineto :: Floating u => Point2 u -> PathM u ()
lineto pt = PathM $ \(St p0 ac) -> ((), St pt (ac `mappend` line p0 pt))

rlineto :: Floating u => Vec2 u -> PathM u ()
rlineto (V2 dx dy) = tip >>= \(P2 x y) -> lineto (P2 (x+dx) (y+dy))


hline :: Floating u => u -> PathM u ()
hline len = snocline (hvec len) 

vline :: Floating u => u -> PathM u ()
vline len = snocline (vvec len) 



bezierto :: (Floating u, Ord u) 
         => Point2 u -> Point2 u -> Point2 u -> PathM u ()
bezierto c1 c2 ep = PathM $ \(St p0 ac) -> 
    ((), St ep (ac `mappend` curve p0 c1 c2 ep))





--


curveto :: (Floating u, Ord u) 
        => Radian -> Radian -> Point2 u -> PathM u ()
curveto cin cout end = PathM $ \(St p0 ac) -> 
    let seg  = curveByAngles p0 cin cout end 
        ac1  = ac `mappend` seg
        end1 = fromMaybe end $ tipR ac1
    in ((), St end1 ac1) 


curveByAngles :: (Floating u, Ord u) 
              => Point2 u -> Radian -> Radian -> Point2 u -> Path u
curveByAngles start cin cout end = curve start (start .+^ v1) (end .+^ v2) end
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

