{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Drawing.Paths.Base.RelBuilder
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Build relative paths monadically.
--
-- \*\* WARNING \*\* this module is an experiment, and may 
-- change significantly or even be dropped from future revisions.
-- 
--------------------------------------------------------------------------------

module Wumpus.Drawing.Paths.Base.RelBuilder
  ( 

    RelBuild
  , runRelBuild
  , execRelBuild
  , evalRelBuild


  , tip 
  , rlineto
  , rcurveto  
  , rmoveto

  , insert
  , vamp
  , cycle

  ) where

-- import qualified Wumpus.Drawing.Paths.Base.AbsPath as A
import Wumpus.Drawing.Paths.Base.BuildCommon
import Wumpus.Drawing.Paths.Base.RelPath
import qualified Wumpus.Drawing.Paths.Base.RelPath as R


import Wumpus.Basic.Kernel                      -- package: wumpus-basic

import Wumpus.Core                              -- package: wumpus-core


import Data.VectorSpace                         -- package: vector-space

import Control.Applicative
import Data.Monoid
import Prelude hiding ( null, log, cycle )


data St u = St 
      { cumulative_disp   :: Vec2 u
      , cumulative_path   :: RelPath u
      , active_path       :: (Vec2 u, RelPath u)
      }

type instance DUnit (St u) = u

type Log u  = BuildLog (LocGraphic u)



 
-- Don\'t want to write pen trace along with the insert commands 
-- as some renderings (fill) should ignore the the pen trace.


-- | Evaluation is two States - a vector for cummulative 
-- displacement and a cummulative path - plus one Writer - a trace 
-- of TikZ-like @insert@ commands.
--
data RelBuild u a = RelBuild { getRelBuild :: St u -> (a, St u, Log u) }

type instance DUnit (RelBuild u a) = u


--------------------------------------------------------------------------------
-- instances



instance Functor (RelBuild u) where
  fmap f mf = RelBuild $ \s0 -> let (a, s1, w) = getRelBuild mf s0
                             in (f a, s1, w)


instance Applicative (RelBuild u) where
  pure a    = RelBuild $ \s0 -> (a, s0, mempty)
  mf <*> ma = RelBuild $ \s0 -> 
                let (f,s1,w1) = getRelBuild mf s0
                    (a,s2,w2) = getRelBuild ma s1
                in (f a, s2, w1 `mappend` w2)

instance Monad (RelBuild u) where
  return a  = RelBuild $ \s0 -> (a, s0, mempty)
  ma >>= k  = RelBuild $ \s0 -> 
                let (a,s1,w1) = getRelBuild ma s0
                    (b,s2,w2) = (getRelBuild . k) a s1
                in (b, s2, w1 `mappend` w2)



initSt :: Num u => Vec2 u -> St u
initSt v = St { cumulative_disp   = v
              , cumulative_path   = emptyRelPath
              , active_path       = (v, emptyRelPath)
              }



-- | Note - runAbsBuild drops the monadic answer and returns the
-- constructed path and a trace of the inserts and sub-paths.
--
runRelBuild :: (Floating u, InterpretUnit u)
            => RelBuild u a -> (RelPath u, LocGraphic u)
runRelBuild mf = post $ getRelBuild mf (initSt $ V2 0 0)
  where
    post (_,st,log) = (cumulative_path st, pen `oplus` ins)
      where
        (v1,sub_last) = active_path st
        log_last      = logSubPath PATH_OPEN id v1 sub_last
        log2          = log `mappend` log_last
        (pen,ins)     = extractTrace emptyLocGraphic log2


-- | Run an 'RelBuild' - return the LocGraphic formed by the pen 
-- trace and the insert trace, /forget/ the outline of the path.
-- 
execRelBuild :: (Floating u, InterpretUnit u)
             => RelBuild u a -> LocGraphic u
execRelBuild mf = snd $ runRelBuild mf



-- | Run an 'RelBuild' - return the outline of the path, /forget/
-- the LocGraphic formed by the pen trace and the insert trace.
-- 
evalRelBuild :: (Floating u, InterpretUnit u)
             => RelBuild u a -> RelPath u
evalRelBuild mf = fst $ runRelBuild mf



logSubPath :: InterpretUnit u 
           => PathEnd -> DrawingContextF -> Vec2 u -> RelPath u -> Log u 
logSubPath spe upd v1 subp 
    | R.null subp  = mempty
    | otherwise    = pen1 gf
  where
    drawF = if spe == PATH_OPEN then openStroke else closedStroke
    gf    = promoteR1 $ \pt -> 
              toPrimPath (displaceVec v1 pt) subp >>= \pp -> 
              localize upd (drawF pp)


tellSubClosed :: InterpretUnit u 
              => DrawingContextF -> Vec2 u -> RelPath u -> RelBuild u ()
tellSubClosed upd v1 subp = 
    RelBuild $ \s0 -> ((), s0, logSubPath PATH_CLOSED upd v1 subp)

tellSubOpen :: InterpretUnit u 
            => DrawingContextF -> Vec2 u -> RelPath u -> RelBuild u ()
tellSubOpen upd v1 subp = 
    RelBuild $ \s0 -> ((), s0, logSubPath PATH_OPEN upd v1 subp)


tellInsert :: LocGraphic u -> RelBuild u ()
tellInsert g1 = 
    RelBuild $ \s0 -> ((),s0, insert1 g1)


sets_ :: (St u -> St u) -> RelBuild u ()
sets_ f = RelBuild $ \s0  -> ((), f s0, mempty)


gets :: (St u -> a) -> RelBuild u a
gets f = RelBuild $ \s0 -> (f s0, s0, mempty)



--------------------------------------------------------------------------------
-- operations

tip :: RelBuild u (Vec2 u)
tip = gets cumulative_disp



-- | Helper - extend the path.
--
extendPath :: Num u 
           => (Vec2 u -> RelPath u -> RelPath u) -> Vec2 u -> RelBuild u ()
extendPath fn v1 = sets_ upd
  where
    upd = (\s v0 i j -> s { cumulative_disp  = v0 ^+^ v1
                          , cumulative_path  = fn v0 i
                          , active_path      = bimapR (fn v0) j })
           <*> cumulative_disp <*> cumulative_path <*> active_path


rlineto :: Floating u => Vec2 u -> RelBuild u ()
rlineto v1 = extendPath (\_ acc -> acc `append` lineTo v1) v1

rcurveto :: Floating u 
         => Vec2 u -> Vec2 u -> Vec2 u -> RelBuild u ()
rcurveto v1 v2 v3 = extendPath (\_ acc -> acc `append` curveTo v1 v2 v3) v3



-- | 'rmoveto' is a pen up.
--
rmoveto :: (Floating u, InterpretUnit u)
        => Vec2 u -> RelBuild u ()
rmoveto v1 = 
    gets active_path >>= \(v0,ans) -> tellSubOpen id v0 ans >> sets_ upd 
  where
    upd   = (\s v0 i -> s { cumulative_disp = v0 ^+^ v1
                          , cumulative_path = i `append` lineTo v1
                          , active_path     = (v0 ^+^ v1, mempty) })
              <*> cumulative_disp <*> cumulative_path


insert :: Num u => LocGraphic u -> RelBuild u ()
insert gf = gets cumulative_disp >>= \v -> 
            tellInsert (moveStart (displaceVec v) gf)



-- Note - vamps should be a data type then we can have libraries 
-- of them.

vamp :: (Floating u, Ord u, Tolerance u, InterpretUnit u) 
     => Vamp u -> RelBuild u ()
vamp (Vamp vnext upd relp path_end) = 
    gets cumulative_disp >>= \v0 -> rmoveto vnext >> drawF upd v0 relp
  where
    drawF = if path_end == PATH_OPEN then tellSubOpen else tellSubClosed


cycle :: (Floating u, InterpretUnit u) => RelBuild u ()
cycle  = 
    gets cumulative_disp >>= \v1 -> 
    gets active_path     >>= \(start,acc) -> 
    tellSubClosed id start (acc `append` lineTo start) >> 
    sets_ (\s -> s { active_path = (v1, mempty)})

