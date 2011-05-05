{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Drawing.Paths.Base.PathBuilder
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

module Wumpus.Drawing.Paths.Base.PathBuilder
  ( 

    LocTraceM(..) -- re-export

  , PathSpec
  , PathSpecT

  , runPathSpec
  , execPathSpec
  , evalPathSpec

  , runPathSpecT
  , execPathSpecT
  , evalPathSpecT


  , PathOpM(..)


{-
  , vamp
  , cycle

  , setIncline
-}
  -- * Derived operators
  , pen_colour
  , pen_width

  , hline
  , vline
 
  ) where

import Wumpus.Drawing.Basis.TraceLocGraphic
import Wumpus.Drawing.Paths.Base.BuildCommon
import Wumpus.Drawing.Paths.Base.RelPath
import qualified Wumpus.Drawing.Paths.Base.RelPath as R


import Wumpus.Basic.Kernel                      -- package: wumpus-basic

import Wumpus.Core                              -- package: wumpus-core


import Data.VectorSpace                         -- package: vector-space

import Control.Applicative
import Control.Monad
import Data.Monoid
import Prelude hiding ( null, log, cycle )



-- DESIGN NOTE - running Paths within the LocTrace monad would 
-- make for a simpler implementation and arguably a simpler API.
-- 
-- Cons: 
-- 1. Cannot get a cumulative path.
-- 2. Changing pen properties takes more code.
-- 
-- Big problem cannot insert in the middle of a path spec...
-- This makes it a non-starter.
--


-- > path (hline 10 >> vline 20)
--
-- Could do something like:
-- 
-- > strokePath (stroke_colour red) (hline 10 >> vline 20) 
--
-- > localize (stroke_colour red) $ path (hline 10 >> vline 20) 
--
-- Perhaps is seems more /functional/ cf. reader monad running in 
-- contexts rather than state monad updating the /current/ state.
--



-- | The vector part of the @active_path@ is its start point. 
-- This allows cycled paths.
--
data BuildSt u = BuildSt 
      { cumulative_path   :: RelPath u
      , current_incline   :: Radian
      , active_path       :: (Vec2 u, RelPath u)
      , pen_trace         :: LocGraphic u
      , pen_dc_modifier   :: DrawingContextF
      }

type instance DUnit (BuildSt u) = u


newtype PathSpec u a = PathSpec { 
      getPathSpec :: BuildSt u -> LocTrace u (a, BuildSt u) }


newtype PathSpecT u m a = PathSpecT { 
      getPathSpecT :: BuildSt u -> LocTraceT u m (a, BuildSt u) } 



 
-- Don\'t want to write pen trace along with the insert commands 
-- as some renderings (fill) should ignore the the pen trace.


-- | Evaluation is two States - a vector for cumulative 
-- displacement and a cummulative path - plus one Writer - a trace 
-- of TikZ-like @insert@ commands.
--


type instance MonUnit (PathSpec u a) = u
type instance MonUnit (PathSpecT u m a) = u


--------------------------------------------------------------------------------
-- instances


-- Functor

instance Functor (PathSpec u) where
  fmap f mf = PathSpec $ \s0 -> 
                getPathSpec mf s0 >>= \(a,s1) -> return (f a,s1)

instance Monad m => Functor (PathSpecT u m) where
  fmap f mf = PathSpecT $ \s0 -> 
                getPathSpecT mf s0 >>= \(a,s1) -> return (f a,s1)


-- Applicative
                                
instance Applicative (PathSpec u) where
  pure a    = PathSpec $ \s0 -> return (a, s0)
  mf <*> ma = PathSpec $ \s0 -> 
                getPathSpec mf s0 >>= \(f,s1) ->
                getPathSpec ma s1 >>= \(a,s2) ->
                return (f a, s2)

instance Monad m => Applicative (PathSpecT u m) where
  pure a    = PathSpecT $ \s0 -> return (a, s0)
  mf <*> ma = PathSpecT $ \s0 -> 
                getPathSpecT mf s0 >>= \(f,s1) ->
                getPathSpecT ma s1 >>= \(a,s2) ->
                return (f a, s2)


-- Monad

instance Monad (PathSpec u) where
  return a  = PathSpec $ \s0 -> return (a, s0)
  ma >>= k  = PathSpec $ \s0 -> 
                getPathSpec ma s0 >>= \(a,s1) -> (getPathSpec . k) a s1 

instance Monad m => Monad (PathSpecT u m) where
  return a  = PathSpecT $ \s0 -> return (a, s0)
  ma >>= k  = PathSpecT $ \s0 -> 
                getPathSpecT ma s0 >>= \(a,s1) -> (getPathSpecT . k) a s1
            



zeroBuildSt :: InterpretUnit u => BuildSt u
zeroBuildSt = BuildSt { cumulative_path   = mempty
                      , current_incline   = 0
                      , active_path       = (V2 0 0, mempty)
                      , pen_trace         = emptyLocGraphic
                      , pen_dc_modifier   = id
                      }


-- The /full/ versions throw away only parts of the @BuildSt@ .
--


-- | Run a PathSpec - return a five-tuple.
--
-- > (ans, path, end_vector, pen_trace, insert_trace) 
--
-- > ans - is the monadic answer, usually ().
--
-- > path - is the relative path formed by all movements during 
-- > the build. This includes movement where the pen is _up_.
--
-- > end_vector - is the cumulative displacement from the start 
-- > point.
--
-- > pen_trace - is ...
--
-- > insert_trace - 
--
runPathSpec :: (Floating u, InterpretUnit u)
            => PathSpec u a 
            -> (a, RelPath u, Vec2 u, LocGraphic u, LocGraphic u)
runPathSpec mf = 
    post $ runLocTrace $ getPathSpec mf zeroBuildSt 
  where
    post ((a,st),end,ins) = let (ph,g1) = postBuildSt end st 
                            in (a, ph, end, g1, ins)



postBuildSt :: InterpretUnit u 
            => Vec2 u -> BuildSt u -> (RelPath u, LocGraphic u)
postBuildSt vnew = step . logSubPath SUBPATH_OPEN vnew 
  where
    step st = (cumulative_path st, pen_trace st)




-- | Run an 'PathSpec' - return the LocGraphic formed by the pen 
-- trace and the insert trace, /forget/ the outline of the path.
-- 
-- Note - the insert trace is printed above the pen trace in the 
-- z-order.
-- 
execPathSpec :: (Floating u, InterpretUnit u)
             => PathSpec u a -> LocGraphic u
execPathSpec mf = post $ runPathSpec mf
  where
    post (_,_,_,g1,g2) = g1 `oplus` g2



-- | Run an 'PathSpec' - return the outline of the path, /forget/
-- the the pen trace and the insert trace.
-- 
evalPathSpec :: (Floating u, InterpretUnit u)
             => PathSpec u a -> RelPath u
evalPathSpec mf = post $ runPathSpec mf
  where
    post (_,ph,_,_,_) = ph


-- | Transformer version of 'runPathSpec'
--
runPathSpecT :: (Monad m, Floating u, InterpretUnit u)
             => PathSpecT u m a 
             -> m (a, RelPath u, Vec2 u, LocGraphic u, LocGraphic u)
runPathSpecT mf = 
    liftM post $ runLocTraceT $ getPathSpecT mf zeroBuildSt 
  where
    post ((a,st),end,ins) = let (ph,g1) = postBuildSt end st 
                            in (a, ph, end, g1, ins)
    


-- | Transformer version of 'execPathSpec'
--
execPathSpecT :: (Monad m, Floating u, InterpretUnit u)
              => PathSpecT u m a -> m (LocGraphic u)
execPathSpecT mf = liftM post $ runPathSpecT mf
  where
    post (_,_,_,g1,g2) = g1 `oplus` g2


-- | Transformer version of 'evalPathSpec'.
-- 
evalPathSpecT :: (Monad m, Floating u, InterpretUnit u)
              => PathSpecT u m a -> m (RelPath u)
evalPathSpecT mf = liftM post $ runPathSpecT mf
  where
    post (_,ph,_,_,_) = ph



-- BuildSt modifiers.

type BuildStF u = BuildSt u -> BuildSt u 



data SubPath = SUBPATH_OPEN | SUBPATH_CLOSED DrawStyle
  deriving (Eq,Show)

logSubPath :: InterpretUnit u 
           => SubPath -> Vec2 u -> BuildStF u
logSubPath term vnew st@(BuildSt { pen_dc_modifier = upd
                                 , pen_trace       = g0       
                                 , active_path     = (v1,subp) })
    | R.null subp  = st
    | otherwise    = st { pen_trace   = g0 `oplus` gf
                        , active_path = (vnew,mempty) }                        
  where
    drawF = case term of
              SUBPATH_OPEN -> dcOpenPath
              SUBPATH_CLOSED sty -> dcClosedPath sty
    gf    = promoteR1 $ \pt -> 
              toPrimPath (dispVec v1 pt) subp >>= \pp -> 
              localize upd (drawF pp)



-- | Helper - extend the path.
-- 
-- Note - this must be twinned with a @moveBy@ to the wrapped 
-- 'LocTrace' monad.
--
extendPath :: Num u 
           => (RelPath u -> RelPath u) -> BuildStF u
extendPath fn = (\s i j -> s { cumulative_path  = fn i
                             , active_path      = bimapR fn j })
           <*> cumulative_path <*> active_path




--------------------------------------------------------------------------------
-- LocTraceM instances

-- Note - path building does not support forking (LocForkTraceM). 


constStatePlain :: LocTrace u a -> PathSpec u a
constStatePlain mf = PathSpec $ \s0 -> mf >>= \a -> return (a,s0)

constStateTrans :: Monad m => LocTraceT u m a -> PathSpecT u m a
constStateTrans mf = PathSpecT $ \s0 -> mf >>= \a -> return (a,s0)


withStatePlain :: (BuildSt u -> LocTrace u (a, BuildSt u)) -> PathSpec u a
withStatePlain sf = PathSpec $ \s0 -> sf s0 

withStateTrans :: Monad m 
               => (BuildSt u -> LocTraceT u m (a, BuildSt u)) -> PathSpecT u m a
withStateTrans sf = PathSpecT $ \s0 -> sf s0 

moveByAlg :: (LocTraceM m, InterpretUnit u, u ~ MonUnit (m ()) )
          => Vec2 u -> (BuildSt u -> m ((), BuildSt u))
moveByAlg v s0 = moveBy v >> location >>= \vnew -> 
                 let s1 = logSubPath SUBPATH_OPEN vnew s0 in return ((),s1)


-- moveBy becomes a pen up

instance InterpretUnit u => LocTraceM (PathSpec u) where
  insertl a = constStatePlain (insertl a)
  location  = constStatePlain location
  moveBy v  = withStatePlain (moveByAlg v)




instance (Monad m, InterpretUnit u) => LocTraceM (PathSpecT u m) where
  insertl a = constStateTrans (insertl a)
  location  = constStateTrans location
  moveBy v  = withStateTrans (moveByAlg v) 



--------------------------------------------------------------------------------
-- 



lineAlg :: (LocTraceM m, Num u, u ~ MonUnit (m ()) )
        => Vec2 u -> (BuildSt u -> m ((), BuildSt u))
lineAlg v1 s0 = let s1 = extendPath (\acc -> snocLineTo acc v1) s0
                in moveBy v1 >> return ((), s1)


curveAlg :: (LocTraceM m, Num u, u ~ MonUnit (m ()) )
         => Vec2 u -> Vec2 u -> Vec2 u -> (BuildSt u -> m ((), BuildSt u))
curveAlg v1 v2 v3 s0 = 
    let s1 = extendPath (\acc -> snocCurveTo acc v1 v2 v3) s0
    in moveBy (v1 ^+^ v2 ^+^ v3) >> return ((), s1)


penAlg :: (LocTraceM m, InterpretUnit u, u ~ MonUnit (m ()) )
       => DrawingContextF -> (BuildSt u -> m ((), BuildSt u))
penAlg upd s0 = location >>= \vnew -> 
                let s1 = logSubPath SUBPATH_OPEN vnew s0
                in return ((),sf s1)
    where
      sf = (\s i -> s { pen_dc_modifier = upd . i}) <*> pen_dc_modifier


cycleAlg :: (LocTraceM m, InterpretUnit u, u ~ MonUnit (m ()))
         => DrawStyle -> (BuildSt u -> m ((), BuildSt u))
cycleAlg sty s0 = 
    let (vnew,_) = active_path s0
        s1       = logSubPath (SUBPATH_CLOSED sty) vnew s0
    in location >>= \vold -> moveBy (vnew ^-^ vold) >> return ((), s1)


-- | @updatePen@ will draw any in-progress path as an open-stroked
-- line before changing the pen properties.
--
class PathOpM m where
  line         :: u ~ MonUnit (m ()) => Vec2 u -> m ()
  curve        :: u ~ MonUnit (m ()) => Vec2 u -> Vec2 u -> Vec2 u -> m ()
  updatePen    :: DrawingContextF -> m ()
  cycleSubPath :: DrawStyle -> m ()


instance InterpretUnit u => PathOpM (PathSpec u) where
  line v1           = withStatePlain (lineAlg v1)
  curve v1 v2 v3    = withStatePlain (curveAlg v1 v2 v3)
  updatePen upd     = withStatePlain (penAlg upd)
  cycleSubPath sty  = withStatePlain (cycleAlg sty)

instance (Monad m, InterpretUnit u) => PathOpM (PathSpecT u m) where
  line v1           = withStateTrans (lineAlg v1)
  curve v1 v2 v3    = withStateTrans (curveAlg v1 v2 v3) 
  updatePen upd     = withStateTrans (penAlg upd)
  cycleSubPath sty  = withStateTrans (cycleAlg sty)



--------------------------------------------------------------------------------
-- operations


{-



-- Note - vamps should be a data type then we can have libraries 
-- of them.

vamp :: (Floating u, Ord u, Tolerance u, InterpretUnit u) 
     => Vamp u -> PathSpec u ()
vamp (Vamp vnext vstart upd relp path_end) = 
    gets cumulative_disp        >>= \v0 -> 
    gets pen_dc_modifier        >>= \cf ->
    move_tip vnext              >> drawF (upd . cf) (v0 ^+^ vstart) relp
  where
    drawF       = if path_end == PATH_OPEN then tellSubOpen else tellSubClosed
 

cycle :: (Floating u, InterpretUnit u) => PathSpec u ()
cycle = 
    gets cumulative_disp  >>= \v1 -> 
    gets pen_dc_modifier  >>= \cf ->
    gets active_path      >>= \(start,acc) -> 
    tellSubClosed cf start (snocLineTo acc start) >> 
    sets_ (\s -> s { active_path = (v1, mempty)})


setIncline :: Radian -> PathSpec u ()
setIncline ang = sets_ upd
  where
    upd = (\s -> s { current_incline = ang })

-}

--------------------------------------------------------------------------------
-- Derived operators


pen_colour :: PathOpM m
           => RGBi -> m ()
pen_colour rgb = updatePen (stroke_colour rgb)

pen_width  :: PathOpM m 
           => Double -> m ()
pen_width d = updatePen (set_line_width d)



hline :: (PathOpM m, Num u, u ~ MonUnit (m ())) => u -> m ()
hline dx = line (hvec dx)

vline :: (PathOpM m, Num u, u ~ MonUnit (m ())) => u -> m ()
vline dy = line (vvec dy)



