{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Drawing.Paths.Base.AbsBuilder
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Build absolute paths monadically.
--
-- \*\* WARNING \*\* this module is an experiment, and may 
-- change significantly or even be dropped from future revisions.
-- 
--------------------------------------------------------------------------------

module Wumpus.Drawing.Paths.Base.AbsBuilder
  ( 

    AbsBuild
  , runAbsBuild
  , execAbsBuild
  , evalAbsBuild

  , tip

  , lineto
  , curveto
  , moveto

  , rlineto
  , rcurveto
  , rmoveto

  , ctrlcurveto

  , insert
  , vamp
  , cycle

  ) where

import Wumpus.Drawing.Paths.Base.AbsPath
import qualified Wumpus.Drawing.Paths.Base.AbsPath as A
import Wumpus.Drawing.Paths.Base.BuildCommon
import qualified Wumpus.Drawing.Paths.Base.RelPath as R


import Wumpus.Basic.Kernel                      -- package: wumpus-basic

import Wumpus.Core                              -- package: wumpus-core

import Data.AffineSpace                         -- package: vector-space
import Data.VectorSpace

import Control.Applicative hiding ( empty )
import Data.Monoid

import Prelude hiding ( log, cycle )



-- State monad building is quite good - it ameliorates the problem
-- of joining to the end point of an empty path...

data St u = St
      { current_point     :: Point2 u 
      , cumulative_path   :: AbsPath u
      , active_path       :: (Point2 u, AbsPath u)
      }

type instance DUnit (St u) = u

type Log u  = BuildLog (Graphic u)


-- | Absolute Path builder monad.
--
newtype AbsBuild u a = AbsBuild { 
          getAbsBuild :: St u -> (a, St u, Log u) }

type instance DUnit (AbsBuild u a) = u


--------------------------------------------------------------------------------
-- instances



instance Functor (AbsBuild u) where
  fmap f mf = AbsBuild $ \s0 -> let (a,s1,w1) = getAbsBuild mf s0
                                in (f a, s1, w1)


instance Applicative (AbsBuild u) where
  pure a    = AbsBuild $ \s0 -> (a,s0,mempty)
  mf <*> ma = AbsBuild $ \s0 -> let (f,s1,w1) = getAbsBuild mf s0
                                    (a,s2,w2) = getAbsBuild ma s1
                                in (f a,s2,w1 `mappend` w2)

instance Monad (AbsBuild u) where
  return a  = AbsBuild $ \s0 -> (a,s0,mempty)
  m >>= k   = AbsBuild $ \s0 -> let (a,s1,w1) = getAbsBuild m s0
                                    (b,s2,w2) = (getAbsBuild . k) a s1
                                in (b, s2, w1 `mappend` w2)





-- | The initial state is needs the start point.
--
initSt :: Floating u => Point2 u -> St u
initSt pt = St { current_point     = pt
               , cumulative_path   = empty pt
               , active_path       = (pt, empty pt)
               }

-- run  - (path,graphic)
-- exec - graphic
-- eval - path

-- | Note - runAbsBuild drops the monadic answer and returns the
-- constructed path and a trace of the inserts and sub-paths.
--
runAbsBuild :: (Floating u, InterpretUnit u)
            => Point2 u -> AbsBuild u a -> (AbsPath u, Graphic u)
runAbsBuild pt mf = post $ getAbsBuild mf (initSt pt)
  where
    post (_,st,log) = let sub_last  = snd $ active_path st
                          log_last  = logSubPath PATH_OPEN id sub_last
                          log2      = log `mappend` log_last
                          empty_gfx = emptyLocGraphic `at` pt
                          (pen,ins) = extractTrace empty_gfx log2
                      in (cumulative_path st, pen `oplus` ins)


-- | Run an 'AbsBuild' - return the Graphic formed by the pen 
-- trace and the insert trace, /forget/ the outline of the path.
-- 
execAbsBuild :: (Floating u, InterpretUnit u)
             => Point2 u -> AbsBuild u a -> Graphic u
execAbsBuild pt mf = snd $ runAbsBuild pt mf



-- | Run an 'AbsBuild' - return the outline of the path, /forget/
-- the  Graphic formed by the pen trace and the insert trace.
-- 
evalAbsBuild :: (Floating u, InterpretUnit u)
             => Point2 u -> AbsBuild u a -> AbsPath u
evalAbsBuild pt mf = fst $ runAbsBuild pt mf



logSubPath :: InterpretUnit u 
           => PathEnd -> DrawingContextF -> AbsPath u -> Log u 
logSubPath spe upd subp 
    | A.null subp  = mempty
    | otherwise    = pen1 (toPrimPath subp >>= localize upd . drawF)
  where
    drawF = if spe == PATH_OPEN then openStroke else closedStroke



tellSubClosed :: InterpretUnit u 
              => DrawingContextF -> AbsPath u -> AbsBuild u ()
tellSubClosed upd subp = 
    AbsBuild $ \s0 -> ((), s0, logSubPath PATH_CLOSED upd subp)

tellSubOpen :: InterpretUnit u 
            => DrawingContextF -> AbsPath u -> AbsBuild u ()
tellSubOpen upd subp = 
    AbsBuild $ \s0 -> ((), s0, logSubPath PATH_OPEN upd subp)


tellInsert :: Graphic u -> AbsBuild u ()
tellInsert g1 = 
    AbsBuild $ \s0 -> ((),s0, insert1 g1)


sets_   :: (St u -> St u) -> AbsBuild u ()
sets_ f = AbsBuild $ \s0 -> ((), f s0, mempty)


gets    :: (St u -> a) -> AbsBuild u a
gets f  = AbsBuild $ \s0 -> (f s0, s0, mempty)



--------------------------------------------------------------------------------
-- operations

tip :: AbsBuild u (Point2 u)
tip = gets current_point

-- | Helper - extend the path.
--
extendPath :: (Point2 u -> AbsPath u -> AbsPath u) -> Point2 u -> AbsBuild u ()
extendPath fn end_pt = sets_ upd
  where
    upd = (\s pt i j -> s { current_point    = end_pt
                          , cumulative_path  = fn pt i
                          , active_path      = bimapR (fn pt) j })
           <*> current_point <*> cumulative_path <*> active_path

lineto :: Floating u => Point2 u -> AbsBuild u ()
lineto p1 = extendPath (\_ acc -> acc `snocLineTo` p1) p1



curveto :: (Floating u, Ord u, Tolerance u)
        => Point2 u -> Point2 u -> Point2 u -> AbsBuild u ()
curveto p1 p2 p3 = extendPath (\_ acc -> snocCurveTo acc p1 p2 p3) p3



   
-- | 'moveto' is a pen up.
--
moveto :: (Floating u, Ord u, Tolerance u, InterpretUnit u) 
       => Point2 u -> AbsBuild u ()
moveto p1 = 
    gets active_path >>= \(_,ans) -> tellSubOpen id ans >> sets_ upd 
  where
    upd   = (\s i -> s { current_point   = p1
                       , cumulative_path = i `snocLineTo` p1
                       , active_path     = (p1, empty p1) }) 
              <*> cumulative_path


rlineto :: Floating u => Vec2 u -> AbsBuild u ()
rlineto v1 = gets current_point >>= \pt -> lineto (pt .+^ v1)


rcurveto :: (Floating u, Ord u, Tolerance u)
         => Vec2 u -> Vec2 u -> Vec2 u -> AbsBuild u ()
rcurveto v1 v2 v3 = 
    gets current_point >>= \pt -> 
    curveto (pt .+^ v1) (pt .+^ v1 ^+^ v2) (pt .+^ v1 ^+^ v2 ^+^ v3)


rmoveto :: (Floating u, Ord u, Tolerance u, InterpretUnit u) 
        => Vec2 u -> AbsBuild u ()
rmoveto v1 = gets current_point >>= \pt -> moveto (pt .+^ v1)




ctrlcurveto :: (Floating u, Ord u, Tolerance u) 
            => Radian -> Radian -> Point2 u -> AbsBuild u ()
ctrlcurveto cin cout p1 = 
    extendPath (\p0 acc -> acc `append` controlCurve p0 cin cout p1) p1



insert :: Num u => LocGraphic u -> AbsBuild u ()
insert gf = gets current_point >>= \pt -> tellInsert (gf `at` pt)



-- Note - vamps should be a data type then we can have libraries 
-- of them.

vamp :: (Floating u, Ord u, Tolerance u, InterpretUnit u) 
     => Vamp u -> AbsBuild u ()
vamp (Vamp vnext upd relp path_end) = 
    gets current_point >>= \p0 -> 
    moveto (p0 .+^ vnext) >> drawF upd (R.toAbsPath p0 relp)
  where
    drawF = if path_end == PATH_OPEN then tellSubOpen else tellSubClosed

cycle :: (Floating u, InterpretUnit u) => AbsBuild u ()
cycle = 
    gets current_point >>= \pt -> 
    gets active_path   >>= \(start,acc) -> 
    tellSubClosed id (acc `snocLineTo` start) >> 
    sets_ (\s -> s { active_path = (pt, empty pt)})


