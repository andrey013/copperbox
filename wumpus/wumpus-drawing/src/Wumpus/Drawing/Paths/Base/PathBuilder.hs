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

  , Vamp
  , PathTerm(..)
  , makeVamp

  , runPathSpec
  , execPathSpec
  , evalPathSpec

  , runPathSpecT
  , execPathSpecT
  , evalPathSpecT

  , execPivot
  , execPivotT


  , PathOpM(..)


  -- * Derived operators
  , pen_colour
  , pen_width

  , lines

  , hlineto
  , vlineto
  , alineto

 
  ) where

import Wumpus.Drawing.Basis.LocTrace
import Wumpus.Drawing.Paths.Base.RelPath
-- import qualified Wumpus.Drawing.Paths.Base.RelPath as R


import Wumpus.Basic.Kernel                      -- package: wumpus-basic

import Wumpus.Core                              -- package: wumpus-core


import Data.VectorSpace                         -- package: vector-space

import Control.Applicative
import Control.Monad
import Data.Monoid
import Prelude hiding ( null, cycle, lines )






-- | The vector part of the @active_path@ is its start point. 
-- This allows cycled paths.
--
data BuildSt u = BuildSt 
      { cumulative_tip    :: Vec2 u
      , cumulative_path   :: RelPath u
      , current_incline   :: Radian
      , active_path       :: ActivePath u
      , pen_trace         :: LocGraphic u
      , ins_trace         :: LocGraphic u
      , pen_dc_modifier   :: DrawingContextF
      }

      -- TODO - is incline worthwhile?


-- | The vector for @PEN_DOWN@ is the start-point not the current
-- tip. 
-- 
-- Startpoint is needed for cycling a path.
-- 
data ActivePath u = PEN_UP
                  | PEN_DOWN (Vec2 u) (RelPath u)
      



type instance DUnit (BuildSt u) = u
type instance DUnit (ActivePath u) = u


newtype PathSpec u a = PathSpec { 
      getPathSpec :: BuildSt u -> (a, BuildSt u) }


newtype PathSpecT u m a = PathSpecT { 
      getPathSpecT :: BuildSt u -> m (a, BuildSt u) } 


-- Note - splitting the state between BuildSt and the /path tip/
-- in LocTrace as actually detrimental to clarity of the code 
-- below. It would make some sense to add the tip and the insert 
-- trace to @BuildSt@ so everything is in one place.


type instance MonUnit (PathSpec u a) = u
type instance MonUnit (PathSpecT u m a) = u


-- | Vamps...
--
data Vamp u = Vamp 
       { vamp_move :: Vec2 u
       , vamp_path :: RelPath u
       , vamp_term :: PathTerm
       }


type instance DUnit (Vamp u) = u


data PathTerm = SUBPATH_OPEN | SUBPATH_CLOSED DrawStyle
  deriving (Eq,Show)


makeVamp :: Vec2 u -> RelPath u -> PathTerm -> Vamp u
makeVamp v1 ph pe = Vamp { vamp_move = v1
                         , vamp_path = ph
                         , vamp_term = pe  
                         }


--------------------------------------------------------------------------------
-- instances


-- Functor

instance Functor (PathSpec u) where
  fmap f mf = PathSpec $ \s0 -> let (a,s1) = getPathSpec mf s0 in (f a,s1)

instance Monad m => Functor (PathSpecT u m) where
  fmap f mf = PathSpecT $ \s0 -> 
                getPathSpecT mf s0 >>= \(a,s1) -> return (f a,s1)


-- Applicative
                                
instance Applicative (PathSpec u) where
  pure a    = PathSpec $ \s0 -> (a, s0)
  mf <*> ma = PathSpec $ \s0 -> let (f,s1) = getPathSpec mf s0 
                                    (a,s2) = getPathSpec ma s1
                                in (f a, s2)

instance Monad m => Applicative (PathSpecT u m) where
  pure a    = PathSpecT $ \s0 -> return (a, s0)
  mf <*> ma = PathSpecT $ \s0 -> 
                getPathSpecT mf s0 >>= \(f,s1) ->
                getPathSpecT ma s1 >>= \(a,s2) ->
                return (f a, s2)


-- Monad

instance Monad (PathSpec u) where
  return a  = PathSpec $ \s0 -> (a, s0)
  ma >>= k  = PathSpec $ \s0 -> 
                let (a,s1) = getPathSpec ma s0 in (getPathSpec . k) a s1 

instance Monad m => Monad (PathSpecT u m) where
  return a  = PathSpecT $ \s0 -> return (a, s0)
  ma >>= k  = PathSpecT $ \s0 -> 
                getPathSpecT ma s0 >>= \(a,s1) -> (getPathSpecT . k) a s1
            


-- | Make the initial build state.
--
zeroBuildSt :: InterpretUnit u => BuildSt u
zeroBuildSt = BuildSt { cumulative_tip    = V2 0 0
                      , cumulative_path   = mempty
                      , current_incline   = 0
                      , active_path       = PEN_UP
                      , pen_trace         = mempty
                      , ins_trace         = mempty
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
    post $ getPathSpec mf zeroBuildSt 
  where
    post (a,st) = let (ph,end,pen,ins) = postBuildSt st
                  in (a,ph,end,pen,ins)

-- | /Close/ the BuildSt, extracting the values.
--
-- A partly drawn sub path will be added to the pen trace as an
-- open sub path.
--
postBuildSt :: InterpretUnit u 
            => BuildSt u -> (RelPath u, Vec2 u, LocGraphic u, LocGraphic u)
postBuildSt s0 = step (penUp SUBPATH_OPEN s0) 
  where
    step st = ( cumulative_path st
              , cumulative_tip st
              , pen_trace st
              , ins_trace st )




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
    post (_,_,_,g1,g2) = g1 `mappend` g2



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
    liftM post $ getPathSpecT mf zeroBuildSt 
  where
    post (a,st) = let (ph,end,pen,ins) = postBuildSt st
                  in (a,ph,end,pen,ins)


-- | Transformer version of 'execPathSpec'
--
execPathSpecT :: (Monad m, Floating u, InterpretUnit u)
              => PathSpecT u m a -> m (LocGraphic u)
execPathSpecT mf = liftM post $ runPathSpecT mf
  where
    post (_,_,_,g1,g2) = g1 `mappend` g2


-- | Transformer version of 'evalPathSpec'.
-- 
evalPathSpecT :: (Monad m, Floating u, InterpretUnit u)
              => PathSpecT u m a -> m (RelPath u)
evalPathSpecT mf = liftM post $ runPathSpecT mf
  where
    post (_,ph,_,_,_) = ph




-- | Form a \"pivot path\" drawing from two path specifications.
-- The start point of the drawing is the pivot formed by joining
-- the paths.
--
execPivot :: (Floating u, InterpretUnit u)
          => PathSpec u a -> PathSpec u a -> LocGraphic u
execPivot ma mb = moveStart (negateV v) $ pen `mappend` ins
  where
   (v, _, _, pen, ins) = runPathSpec ( ma >> location >>= \ans -> 
                                       mb >> return ans )

-- | Transformer version of 'execPivot'.
-- 
execPivotT :: (Floating u, InterpretUnit u, Monad m)
           => PathSpecT u m a -> PathSpecT u m a -> m (LocGraphic u)
execPivotT ma mb = 
    liftM post $ runPathSpecT ( ma >> location >>= \ans -> 
                                mb >> return ans )
  where
    post (v, _, _, pen, ins) = moveStart (negateV v) $ pen `mappend` ins
       


 
--------------------------------------------------------------------------------

-- BuildSt modifiers.

type BuildStF u = BuildSt u -> BuildSt u 


-- | Helper - extend the path with a line.
-- 
-- This is an implicit PEN_DOWN if the active pen is UP.
--
extendPath :: Floating u 
           => Vec2 u -> BuildStF u
extendPath v1 = (\s v0 ph pa -> s { cumulative_tip   = v0 ^+^ v1
                                  , cumulative_path  = updP ph
                                  , active_path      = updA v0 pa })
           <*> cumulative_tip <*> cumulative_path <*> active_path
   where
     updP ph                   = snocLineTo ph v1
     updA tip PEN_UP           = PEN_DOWN tip (line1 v1)
     updA _   (PEN_DOWN v0 ph) = PEN_DOWN v0 (snocLineTo ph v1)




-- | Helper - extend the path with a curve.
--
-- This is an implicit PEN_DOWN if the active pen is UP.
-- 
extendPathC :: (Floating u, Ord u, Tolerance u)
            => Vec2 u -> Vec2 u -> Vec2 u -> BuildStF u
extendPathC c1 c2 c3 = 
    (\s v0 ph pa -> s { cumulative_tip   = v0 ^+^ c1 ^+^ c2 ^+^ c3
                      , cumulative_path  = updP ph
                      , active_path      = updA v0 pa })
      <*> cumulative_tip <*> cumulative_path <*> active_path
   where
     updP ph                   = snocCurveTo ph c1 c2 c3
     updA tip PEN_UP           = PEN_DOWN tip (curve1 c1 c2 c2)
     updA _   (PEN_DOWN v0 ph) = PEN_DOWN v0 (snocCurveTo ph c1 c2 c3)


-- | Helper - change the active_path to PEN_UP. 
-- 
-- This will implicitly log any partly drawn path.
--
penUp :: InterpretUnit u => PathTerm -> BuildStF u
penUp term = 
    (\s pt pa upd -> s { active_path = PEN_UP
                       , pen_trace   = pt `mappend` fn upd pa })
      <*> pen_trace <*> active_path <*> pen_dc_modifier
  where
    fn _   PEN_UP           = mempty
    fn upd (PEN_DOWN v0 pa) = subPathDraw upd v0 pa term


-- | Move the current tip.
--
-- This is an implicit PEN_UP if the active pen is DOWN.
-- 
moveTip :: (Floating u, InterpretUnit u) 
        => Vec2 u -> BuildStF u
moveTip v1 = 
    (\s pa v0 cp -> let s1 = case pa of PEN_UP -> s; _ -> penUp SUBPATH_OPEN s
                    in s1 { cumulative_tip  = v0 ^+^ v1
                          , cumulative_path = snocLineTo cp v1 })
      <*> active_path <*> cumulative_tip <*> cumulative_path


-- | Cycle the current active path.
--
cycleAP :: (Floating u, InterpretUnit u) 
        => DrawStyle -> BuildStF u
cycleAP sty = 
    (\s pa vtip cp -> case pa of
                        PEN_UP -> s
                        PEN_DOWN v0 _ -> let s1 = penUp (SUBPATH_CLOSED sty) s
                                             mv = v0 ^-^ vtip
                                         in s1 { cumulative_tip  = v0 
                                               , cumulative_path = snocLineTo cp mv })
      <*> active_path <*> cumulative_tip <*> cumulative_path
    
    

-- | Change the drawing props of the current pen.
--
-- This is an implicit PEN_UP if the active pen is DOWN.
-- 
changePen :: InterpretUnit u => DrawingContextF -> BuildStF u
changePen upd = 
    (\s pa df -> let s1 = case pa of PEN_UP -> s; _ -> penUp SUBPATH_OPEN s
                 in s1 { pen_dc_modifier  = (upd . df) })
      <*> active_path <*> pen_dc_modifier



insertGf :: InterpretUnit u => LocGraphic u -> BuildStF u
insertGf gf = 
    (\s ins v1 -> let g1 = moveStart v1 gf
                  in s { ins_trace = ins `mappend` g1 })
      <*> ins_trace <*> cumulative_tip
               


appendVamp :: (Floating u, InterpretUnit u) 
           => Vamp u -> BuildStF u
appendVamp (Vamp { vamp_path = vph, vamp_term = term, vamp_move = mv }) =
    next . penUp SUBPATH_OPEN
  where
    next = (\s v1 cp trc df -> let p1 = subPathDraw df v1 vph term
                               in s { cumulative_tip  = v1 ^+^ mv
                                    , cumulative_path = snocLineTo cp mv
                                    , pen_trace       = trc `mappend` p1 })
            <*> cumulative_tip <*> cumulative_path 
            <*> pen_trace      <*> pen_dc_modifier
                                 


subPathDraw :: InterpretUnit u 
            => DrawingContextF -> Vec2 u -> RelPath u -> PathTerm 
            -> LocGraphic u
subPathDraw upd v0 subp term = promoteLoc $ \pt -> 
    zapQuery (toPrimPath (displace v0 pt) subp) >>= \pp -> localize upd (drawF pp)
  where
    drawF = case term of
              SUBPATH_OPEN -> dcOpenPath
              SUBPATH_CLOSED sty -> dcClosedPath sty





--------------------------------------------------------------------------------
-- LocTraceM instances

-- Note - path building does not support forking (LocForkTraceM). 

-- moveBy becomes a pen up

instance (Floating u, InterpretUnit u) => 
    LocTraceM (PathSpec u) where
  insertl a = PathSpec $ \s0 -> ((), insertGf a s0)
  location  = PathSpec $ \s0 -> (cumulative_tip s0, s0)
  moveBy v  = PathSpec $ \s0 -> ((), moveTip v s0)




instance (Monad m, Floating u, InterpretUnit u) => 
    LocTraceM (PathSpecT u m) where
  insertl a = PathSpecT $ \s0 -> return ((), insertGf a s0)
  location  = PathSpecT $ \s0 -> return (cumulative_tip s0, s0)
  moveBy v  = PathSpecT $ \s0 -> return ((), moveTip v s0)



--------------------------------------------------------------------------------
-- 


-- | @updatePen@ will draw any in-progress path as an open-stroked
-- line before changing the pen properties.
--
class Monad m => PathOpM m where
  lineto       :: u ~ MonUnit (m ()) => Vec2 u -> m ()
  curveto      :: u ~ MonUnit (m ()) => Vec2 u -> Vec2 u -> Vec2 u -> m ()
  updatePen    :: DrawingContextF -> m ()
  cycleSubPath :: DrawStyle -> m ()
  vamp         :: u ~ MonUnit (m ()) => Vamp u -> m ()



instance (Floating u, Ord u, Tolerance u, InterpretUnit u) => 
    PathOpM (PathSpec u) where
  lineto v1         = PathSpec $ \s0 -> ((), extendPath v1 s0)
  curveto v1 v2 v3  = PathSpec $ \s0 -> ((), extendPathC v1 v2 v3 s0)
  updatePen upd     = PathSpec $ \s0 -> ((), changePen upd s0)
  cycleSubPath sty  = PathSpec $ \s0 -> ((), cycleAP sty s0)
  vamp vp           = PathSpec $ \s0 -> ((), appendVamp vp s0)



instance (Monad m, Floating u, Ord u, Tolerance u, InterpretUnit u) => 
    PathOpM (PathSpecT u m) where
  lineto v1         = PathSpecT $ \s0 -> return ((), extendPath v1 s0)
  curveto v1 v2 v3  = PathSpecT $ \s0 -> return ((), extendPathC v1 v2 v3 s0)
  updatePen upd     = PathSpecT $ \s0 -> return ((), changePen upd s0)
  cycleSubPath sty  = PathSpecT $ \s0 -> return ((), cycleAP sty s0)
  vamp vp           = PathSpecT $ \s0 -> return ((), appendVamp vp s0)



--------------------------------------------------------------------------------
-- operations



{-

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

-- | A different name is needed for this one...
--
lines :: (PathOpM m, u ~ MonUnit (m ())) => [Vec2 u] -> m ()
lines = mapM_ lineto


hlineto :: (PathOpM m, Num u, u ~ MonUnit (m ())) => u -> m ()
hlineto dx = lineto (hvec dx)

vlineto :: (PathOpM m, Num u, u ~ MonUnit (m ())) => u -> m ()
vlineto dy = lineto (vvec dy)


alineto :: (PathOpM m, Floating u, u ~ MonUnit (m ())) => Radian -> u -> m ()
alineto ang d = lineto (avec ang d)
