{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Drawing.Paths.PathBuilder
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

module Wumpus.Drawing.Paths.PathBuilder
  ( 


    GenPathSpec
  , PathSpec
  , Vamp(..)


  , runGenPathSpec
  , execGenPathSpec
  , evalGenPathSpec
  , stripGenPathSpec

  , runPathSpec
  , runPathSpec_

  , runPivot


  , penline
  , pencurve
 
  
  , breakPath
  , hpenline
  , vpenline
  , apenline

  , penlines
  , pathmoves

  , vamp
  , cycleSubPath
  , updatePen
 
  ) where

import Wumpus.Drawing.Paths.Base

import Wumpus.Basic.Kernel                      -- package: wumpus-basic

import Wumpus.Core                              -- package: wumpus-core


import Data.AffineSpace                         -- package: vector-space
import Data.VectorSpace

import Control.Applicative
import Control.Monad
import Data.Monoid
import Prelude hiding ( null, cycle, lines )

-- 
-- TODO - possibly we need two drawing contexts one for the pen 
-- and one for the decoration trace.
-- 
-- Alternatively PathSt should have a local DrawingContext for the 
-- pen.
--


-- | Note - a path spec has an immutable start point like 
-- @LocDrawing@.
--
-- Effectively a path is draw in a local coordinate system with 
-- @(0,0)@ as the origin.
--
newtype GenPathSpec st u a = GenPathSpec { 
    getGenPathSpec :: DrawingContext -> PathSt st -> (a, PathSt st, CatPrim) }

type instance DUnit   (GenPathSpec st u a) = u
type instance UState  (GenPathSpec st u)   = st

type PathSpec u a = GenPathSpec () u a


data PathSt st = PathSt
      { st_active_pen       :: ActivePen
      , st_pen_ctx          :: DrawingContext
      , st_cumulative_path  :: AbsPath Double
      , st_user_state       :: st
      }


-- | Note - this formulation doesn\'t support monoidal append.
-- 
-- Information gets lost for this one (we really would want to 
-- draw the left-hand-side):
--
-- > PEN_DOWN _ _ `mappend` PEN_UP
--
-- So it has to be part of the state not the writer.
-- 
data ActivePen = PEN_UP 
               | PEN_DOWN (AbsPath Double)
                         


zeroActivePen :: DPoint2 -> ActivePen
zeroActivePen pt = PEN_DOWN (emptyPath pt)


data Vamp u = Vamp
       { vamp_move :: Vec2 u
       , vamp_conn :: ConnectorGraphic u
       }

type instance DUnit (Vamp u) = u


--------------------------------------------------------------------------------
-- Instances


-- Functor

instance Functor (GenPathSpec st u) where
  fmap f ma = GenPathSpec $ \ctx s -> 
                let (a,s1,w1) = getGenPathSpec ma ctx s 
                in (f a,s1,w1)


-- Applicative

instance Applicative (GenPathSpec st u) where
  pure a    = GenPathSpec $ \_   s -> (a, s, mempty)
  mf <*> ma = GenPathSpec $ \ctx s -> 
                let (f,s1,w1) = getGenPathSpec mf ctx s
                    (a,s2,w2) = getGenPathSpec ma ctx s1
                in (f a, s2, w1 `mappend` w2)


-- Monad

instance Monad (GenPathSpec st u) where
  return a  = GenPathSpec $ \_   s -> (a, s, mempty)
  ma >>= k  = GenPathSpec $ \ctx s -> 
                let (a,s1,w1) = getGenPathSpec ma ctx s
                    (b,s2,w2) = (getGenPathSpec . k) a ctx s1
                in (b, s2, w1 `mappend` w2)


-- Monoid 

instance Monoid a => Monoid (GenPathSpec st u a) where
  mempty           = GenPathSpec $ \_   s -> (mempty, s, mempty)
  ma `mappend` mb  = GenPathSpec $ \ctx s -> 
                       let (a,s1,w1) = getGenPathSpec ma ctx s
                           (b,s2,w2) = getGenPathSpec mb ctx s1
                       in (a `mappend` b, s2, w1 `mappend` w2)


-- DrawingCtxM

instance DrawingCtxM (GenPathSpec st u) where
  askDC           = GenPathSpec $ \ctx s -> (ctx, s, mempty)
  asksDC f        = GenPathSpec $ \ctx s -> (f ctx, s, mempty)
  localize upd ma = GenPathSpec $ \ctx s -> 
                      getGenPathSpec ma (upd ctx) s


-- UserStateM 

instance UserStateM (GenPathSpec st u) where
  getState        = GenPathSpec $ \_ s -> 
                      (st_user_state s, s, mempty)
  setState ust    = GenPathSpec $ \_ s -> 
                      ((), s {st_user_state = ust} , mempty)
  updateState upd = GenPathSpec $ \_ s -> 
                      let ust = st_user_state s
                      in ((), s {st_user_state =  upd ust}, mempty)



-- Note - all these need to peek at the cumulative path

-- LocationM

instance InterpretUnit u => LocationM (GenPathSpec st u) where
  location = locationImpl


-- CursorM 

instance InterpretUnit u => CursorM (GenPathSpec st u) where
  moveby   = movebyImpl


-- InsertlM

instance InterpretUnit u => InsertlM (GenPathSpec st u) where
  insertl  = insertlImpl


           
--------------------------------------------------------------------------------
-- Run functions

runGenPathSpec :: InterpretUnit u 
               => st -> PathMode -> GenPathSpec st u a 
               -> LocImage u (a, st, AbsPath u)
runGenPathSpec st mode ma = promoteLoc $ \pt -> 
    askDC >>= \ctx ->
    let P2 dx dy  = normalizeF (dc_font_size ctx) pt
        st_zero   = PathSt (zeroActivePen zeroPt) ctx (emptyPath zeroPt) st
        (a,s1,w1) = getGenPathSpec ma ctx st_zero
        dpath     = translate dx dy $ st_cumulative_path s1
        upath     = dinterpF (dc_font_size ctx) dpath
        pctx      = st_pen_ctx s1
        (_,w2)    = runImage pctx (drawActivePen mode $ st_active_pen s1) 
        wfinal    = cpmove (V2 dx dy) $ w1 `mappend` w2
    in replaceAns (a, st_user_state s1, upath) $ primGraphic wfinal


-- Note - eval and exec return the AbsPath this is as-per RWS
-- which returns @w@ for execRWS (s,w) and evalRWS (a,w)
--

evalGenPathSpec :: InterpretUnit u
                => st -> PathMode -> GenPathSpec st u a
                -> LocImage u (a, AbsPath u)
evalGenPathSpec st mode ma = 
    (\(a,_,w) -> (a,w)) <$> runGenPathSpec st mode ma

    

execGenPathSpec :: InterpretUnit u
                => st -> PathMode -> GenPathSpec st u a
                -> LocImage u (st, AbsPath u)
execGenPathSpec st mode ma =
    (\(_,s,w) -> (s,w)) <$> runGenPathSpec st mode ma



stripGenPathSpec :: InterpretUnit u
                 => st -> PathMode -> GenPathSpec st u a
                 -> LocQuery u (a, st, AbsPath u)
stripGenPathSpec st mode ma = stripLocImage $ runGenPathSpec st mode ma


runPathSpec :: InterpretUnit u
            => PathMode -> PathSpec u a -> LocImage u (a, AbsPath u)
runPathSpec mode ma = evalGenPathSpec () mode ma

runPathSpec_ :: InterpretUnit u
             => PathMode -> PathSpec u a -> LocGraphic u
runPathSpec_ mode ma = ignoreAns $ evalGenPathSpec () mode ma



-- Monad run function nomenclature:
--
-- > run  - both
-- > eval - answer (no state)
-- > exec - state (no answer)
-- 
-- Note RWS always returns the @w@.
--
-- For Wumpus:
-- 
-- > run  - monadic answer, and the writer /construction/
-- > eval - just the monadic answer
-- > exec - just the writer /construction/.
--
-- In all case the CatPrim inside the LocImage may contain 
-- additional graphics.
--
-- Client code can use @ignoreAns@ to generate a @LocGraphic@
-- from the @LocImage@.



-- | Helper.
--
drawActivePen :: PathMode -> ActivePen -> DGraphic 
drawActivePen _    PEN_UP              = mempty
drawActivePen mode (PEN_DOWN abs_path) = drawPath_ mode abs_path







-- | Form a \"pivot path\" drawing from two path specifications.
-- The start point of the drawing is the pivot formed by joining
-- the paths.
--
runPivot :: (Floating u, InterpretUnit u)
          => PathSpec u a -> PathSpec u a -> LocGraphic u
runPivot ma mb = promoteLoc $ \pt -> 
    askDC >>= \ctx ->
    let dpt        = normalizeF (dc_font_size ctx) pt
        st_zero    = PathSt (zeroActivePen zeroPt) ctx (emptyPath zeroPt) ()
        (p1,s1,w1) = getGenPathSpec mz ctx st_zero
        dp1        = normalizeF (dc_font_size ctx) p1
        v1         = pvec dpt dp1
        pctx       = st_pen_ctx s1
        (_,w2)     = runImage pctx $ drawActivePen OSTROKE $ st_active_pen s1
        wfinal     = w1 `mappend` w2
    in primGraphic $ cpmove (negateV v1) wfinal
  where
    mz = ma >> location >>= \pt -> mb >> return pt


--------------------------------------------------------------------------------
-- operations


locationImpl :: InterpretUnit u => GenPathSpec st u (Point2 u)
locationImpl = GenPathSpec $ \ctx s ->
    let pt  = tipR $ st_cumulative_path s 
        upt = dinterpF (dc_font_size ctx) pt
    in (upt, s, mempty)


-- | 'extendPaths' extends both the @cumulative_path@ and the 
-- @active_pen@. If the pen is up it, changes to a pendown.
--
extendPaths :: DVec2 -> PathSt st -> PathSt st
extendPaths v1 s@(PathSt { st_cumulative_path = cp
                         , st_active_pen      = pen} )  = 
    s { st_cumulative_path = snocLine cp v1, st_active_pen = upd pen }
  where
    upd PEN_UP          = let pt = tipR cp in PEN_DOWN $ line1 pt (pt .+^ v1)
    upd (PEN_DOWN absp) = PEN_DOWN $ snocLine absp v1




-- | Extend the path with a line, drawn by the pen.
-- 
penline :: InterpretUnit u => Vec2 u -> GenPathSpec st u ()
penline v1 = GenPathSpec $ \ctx s -> 
   let sz  = dc_font_size ctx
       dv1 = normalizeF sz v1
   in ((), extendPaths dv1 s, mempty)



-- | @extendPenC@ causes a pendown.
--
extendPathsC :: DVec2 -> DVec2 -> DVec2 -> PathSt st -> PathSt st
extendPathsC v1 v2 v3 s@(PathSt { st_cumulative_path = cp
                                , st_active_pen      = pen} )  = 
    s { st_cumulative_path = snocCurve cp (v1,v2,v3), st_active_pen = upd pen }
  where
    upd PEN_UP          = let p0 = tipR cp 
                              p1 = p0 .+^ v1
                              p2 = p1 .+^ v2
                              p3 = p2 .+^ v3
                          in PEN_DOWN $ curve1 p0 p1 p2 p3
    upd (PEN_DOWN absp) = PEN_DOWN $ snocCurve absp (v1,v2,v3)



-- | Extend the path with a curve, drawn by the pen.
-- 
pencurve :: InterpretUnit u 
        => Vec2 u -> Vec2 u -> Vec2 u -> GenPathSpec st u ()
pencurve v1 v2 v3 = GenPathSpec $ \ctx s -> 
   let sz  = dc_font_size ctx
       dv1 = normalizeF sz v1
       dv2 = normalizeF sz v2
       dv3 = normalizeF sz v3
   in ((), extendPathsC dv1 dv2 dv3 s, mempty)


-- | @moveby@ causes a pen up.
--
movebyImpl :: InterpretUnit u => Vec2 u -> GenPathSpec st u ()
movebyImpl v1 = GenPathSpec $ \ctx s@(PathSt {st_pen_ctx = pctx}) ->
    let sz      = dc_font_size ctx
        dv1     = normalizeF sz v1
        (_,w1)  = runImage pctx $ drawActivePen OSTROKE $ st_active_pen s
        cpath   = snocLine (st_cumulative_path s) dv1
    in ((), s { st_active_pen = PEN_UP, st_cumulative_path = cpath }, w1)


breakPath :: InterpretUnit u => GenPathSpec st u ()
breakPath = movebyImpl (V2 0 0)

hpenline :: InterpretUnit u => u -> GenPathSpec st u ()
hpenline dx = penline (hvec dx)

vpenline :: InterpretUnit u => u -> GenPathSpec st u ()
vpenline dy = penline (vvec dy)


apenline :: (Floating u, InterpretUnit u) 
         => Radian -> u -> GenPathSpec st u ()
apenline ang d = penline (avec ang d)

penlines :: InterpretUnit u => [Vec2 u] -> GenPathSpec st u ()
penlines = mapM_ penline

pathmoves :: InterpretUnit u => [Vec2 u] -> GenPathSpec st u ()
pathmoves = mapM_ moveby



insertlImpl :: InterpretUnit u 
            => LocImage u a -> GenPathSpec st u a
insertlImpl gf = GenPathSpec $ \ctx s ->
    let upt     = dinterpF (dc_font_size ctx) (tipR $ st_cumulative_path s)
        (a,wcp) = runLocImage ctx upt gf
    in (a, s, wcp)



vamp :: InterpretUnit u => Vamp u -> GenPathSpec st u ()
vamp (Vamp v1 conn) = GenPathSpec $ \ctx s@(PathSt {st_pen_ctx = pctx}) ->
    let sz     = dc_font_size ctx
        dv1    = normalizeF sz v1
        (_,w1) = runImage pctx $ drawActivePen OSTROKE $ st_active_pen s
        upt    = dinterpF sz (tipR $ st_cumulative_path s)
        (_,w2) = runConnectorImage ctx upt (upt .+^ v1) conn
        cpath  = snocLine (st_cumulative_path s) dv1
    in ((), s { st_active_pen = PEN_UP, st_cumulative_path = cpath }
          , w1 `mappend` w2)


cycleSubPath :: DrawMode -> GenPathSpec st u ()
cycleSubPath mode = GenPathSpec $ \_ s@(PathSt {st_pen_ctx = pctx}) ->
    let (_,w1) = runImage pctx $ drawActivePen (fn mode) (st_active_pen s)
    in ((), s { st_active_pen = PEN_UP }, w1)
  where
    fn DRAW_STROKE      = CSTROKE
    fn DRAW_FILL        = CFILL
    fn DRAW_FILL_STROKE = CFILL_STROKE



-- Design note 
--
-- Should pen changing be @local@ style vis the Reader monad or a 
-- state change with the State monad?
-- 
-- Now switched to state change.
--


-- | Note - updates the pen but doesn\'t draw, the final path
-- will be drawing with the last updated context.
--
updatePen :: DrawingContextF -> GenPathSpec st u ()
updatePen upd = GenPathSpec $ \_ s@(PathSt { st_pen_ctx = pctx})  ->
    ((), s { st_pen_ctx = upd pctx}, mempty )

