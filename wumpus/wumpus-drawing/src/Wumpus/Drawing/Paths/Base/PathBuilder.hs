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


    GenPathSpec
  , PathSpec
  , Vamp(..)
  , PathTerm(..)

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
  , localPen
 
  ) where

import Wumpus.Drawing.Paths.Base.RelPath


import Wumpus.Basic.Kernel                      -- package: wumpus-basic

import Wumpus.Core                              -- package: wumpus-core


import Data.AffineSpace                         -- package: vector-space
import Data.VectorSpace

import Control.Applicative
import Control.Monad
import Data.Monoid
import Prelude hiding ( null, cycle, lines )


newtype GenPathSpec st u a = GenPathSpec
          { getGenPathSpec :: DrawingContext -> DPoint2 -> PathSt st
                           -> (a, DPoint2, PathSt st, PathW) }

type instance DUnit   (GenPathSpec st u a) = u
type instance UState  (GenPathSpec st u)   = st

type PathSpec u a = GenPathSpec () u a


data PathSt st = PathSt
      { st_active_pen :: ActivePen
      , st_user_state :: st
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
               | PEN_DOWN { ap_start_point  :: Point2 Double
                          , ap_rel_path     :: RelPath Double 
                          }


zeroActivePath :: DPoint2 -> ActivePen
zeroActivePath pt = PEN_DOWN pt mempty


data PathW = PathW 
      { w_rel_path :: RelPath Double
      , w_trace    :: CatPrim
      }


instance Monoid PathW where
  mempty = PathW mempty mempty
  PathW a0 b0 `mappend` PathW a1 b1 = PathW (a0 `mappend` a1) (b0 `mappend` b1)



data PathTerm = PATH_OPEN | PATH_CLOSED DrawStyle
  deriving (Eq,Show)


data Vamp u = Vamp
       { vamp_move :: Vec2 u
       , vamp_conn :: ConnectorGraphic u
       }

type instance DUnit (Vamp u) = u


--------------------------------------------------------------------------------
-- Instances


-- Functor

instance Functor (GenPathSpec st u) where
  fmap f ma = GenPathSpec $ \ctx pt s -> 
                let (a,p1,s1,w1) = getGenPathSpec ma ctx pt s 
                in (f a,p1,s1,w1)


-- Applicative

instance Applicative (GenPathSpec st u) where
  pure a    = GenPathSpec $ \_   pt s -> (a, pt, s, mempty)
  mf <*> ma = GenPathSpec $ \ctx pt s -> 
                let (f,p1,s1,w1) = getGenPathSpec mf ctx pt s
                    (a,p2,s2,w2) = getGenPathSpec ma ctx p1 s1
                in (f a, p2, s2, w1 `mappend` w2)


-- Monad

instance Monad (GenPathSpec st u) where
  return a  = GenPathSpec $ \_   pt s -> (a, pt, s, mempty)
  ma >>= k  = GenPathSpec $ \ctx pt s -> 
                let (a,p1,s1,w1) = getGenPathSpec ma ctx pt s
                    (b,p2,s2,w2) = (getGenPathSpec . k) a ctx p1 s1
                in (b, p2, s2, w1 `mappend` w2)


instance Monoid a => Monoid (GenPathSpec st u a) where
  mempty           = GenPathSpec $ \_   pt s -> (mempty, pt, s, mempty)
  ma `mappend` mb  = GenPathSpec $ \ctx pt s -> 
                       let (a,p1,s1,w1) = getGenPathSpec ma ctx pt s
                           (b,p2,s2,w2) = getGenPathSpec mb ctx p1 s1
                       in (a `mappend` b, p2, s2, w1 `mappend` w2)

-- DrawingCtxM

instance DrawingCtxM (GenPathSpec st u) where
  askDC           = GenPathSpec $ \ctx pt s -> (ctx, pt, s, mempty)
  asksDC f        = GenPathSpec $ \ctx pt s -> (f ctx, pt, s, mempty)
  localize upd ma = GenPathSpec $ \ctx pt s -> 
                      getGenPathSpec ma (upd ctx) pt s


-- UserStateM 

instance UserStateM (GenPathSpec st u) where
  getState        = GenPathSpec $ \_ pt s -> 
                      (st_user_state s, pt, s, mempty)
  setState ust    = GenPathSpec $ \_ pt s -> 
                      ((), pt, s {st_user_state = ust} , mempty)
  updateState upd = GenPathSpec $ \_ pt s -> 
                      let ust = st_user_state s
                      in ((), pt, s {st_user_state =  upd ust}, mempty)



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
               => GenPathSpec st u a -> st -> PathTerm 
               -> LocImage u (a, st, RelPath u)
runGenPathSpec ma st term = promoteLoc $ \pt -> 
    askDC >>= \ctx ->
    let dpt       = normalizeF (dc_font_size ctx) pt
        st_zero   = PathSt (zeroActivePath dpt) st
        (a,_,s,w) = getGenPathSpec ma ctx dpt st_zero
        upath     = dinterpF (dc_font_size ctx) $ w_rel_path w
        (_,wcp)   = runImage (drawActivePen term $ st_active_pen s) ctx
        wfinal    = w_trace w `mappend` wcp
    in replaceAns (a, st_user_state s, upath) $ primGraphic wfinal


-- Note - eval and exec return the RelPath this is as-per RWS
-- which returns @w@ for execRWS (s,w) and evalRWS (a,w)
--

evalGenPathSpec :: InterpretUnit u
                => GenPathSpec st u a -> st -> PathTerm 
                -> LocImage u (a, RelPath u)
evalGenPathSpec ma st term = 
    (\(a,_,w) -> (a,w)) <$> runGenPathSpec ma st term

    

execGenPathSpec :: InterpretUnit u
                => GenPathSpec st u a -> st -> PathTerm 
                -> LocImage u (st, RelPath u)
execGenPathSpec ma st term =
    (\(_,s,w) -> (s,w)) <$> runGenPathSpec ma st term



stripGenPathSpec :: InterpretUnit u
                 => GenPathSpec st u a -> st -> PathTerm 
                 -> LocQuery u (a, st, RelPath u)
stripGenPathSpec ma st term = stripLocImage $ runGenPathSpec ma st term


runPathSpec :: InterpretUnit u
            => PathSpec u a -> PathTerm -> LocImage u (a, RelPath u)
runPathSpec ma term = evalGenPathSpec ma () term

runPathSpec_ :: InterpretUnit u
             => PathSpec u a -> PathTerm -> LocGraphic u
runPathSpec_ ma term = ignoreAns $ evalGenPathSpec ma () term



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
drawActivePen :: PathTerm -> ActivePen -> DGraphic 
drawActivePen _    PEN_UP                            = mempty
drawActivePen term (PEN_DOWN { ap_start_point = pt
                             , ap_rel_path    = rp}) = case term of
    PATH_OPEN -> ignoreAns $ drawOpenPath rp `at` pt
    PATH_CLOSED styl -> ignoreAns $ drawClosedPath styl rp `at` pt








-- | Form a \"pivot path\" drawing from two path specifications.
-- The start point of the drawing is the pivot formed by joining
-- the paths.
--
runPivot :: (Floating u, InterpretUnit u)
          => PathSpec u a -> PathSpec u a -> LocGraphic u
runPivot ma mb = promoteLoc $ \pt -> 
    askDC >>= \ctx ->
    let dpt         = normalizeF (dc_font_size ctx) pt
        st_zero     = PathSt (zeroActivePath dpt) ()
        (p1,_,s,w1) = getGenPathSpec mz ctx dpt st_zero
        dp1         = normalizeF (dc_font_size ctx) p1
        v1          = pvec dpt dp1
        (_,wcp)     = runImage (drawActivePen PATH_OPEN $ st_active_pen s) ctx
        wfinal      = w_trace w1 `mappend` wcp        
    in primGraphic $ cpmove (negateV v1) wfinal
  where
    mz = ma >> location >>= \pt -> mb >> return pt


--------------------------------------------------------------------------------
-- operations


locationImpl :: InterpretUnit u => GenPathSpec st u (Point2 u)
locationImpl = GenPathSpec $ \ctx pt s ->
    let upt = dinterpF (dc_font_size ctx) pt
    in (upt, pt, s, mempty)


-- | @extendPen@ causes a pendown.
--
extendPen :: DPoint2 -> DVec2 -> ActivePen -> ActivePen
extendPen pt v PEN_UP           = PEN_DOWN pt (line1 v)
extendPen _  v (PEN_DOWN p0 rp) = PEN_DOWN p0 (rp `snocLineTo` v)

--
-- NOTE - /lineto/ needs a new name as the @to@ suffix suggests
-- moving to a point, whereas the movement is @by@ a vector.
--
-- @relline@ not a good candidate, need a name that supports 
-- horizontal (h) and vertical (v) versions.
--


-- | Extend the path with a line, drawn by the pen.
-- 
penline :: InterpretUnit u => Vec2 u -> GenPathSpec st u ()
penline v1 = GenPathSpec $ \ctx pt s -> 
   let sz  = dc_font_size ctx
       dv1 = normalizeF sz v1
       pen = extendPen pt dv1 (st_active_pen s)
       w1  = PathW { w_rel_path = line1 dv1, w_trace = mempty }
   in ((), pt .+^ dv1, s { st_active_pen = pen }, w1)



-- | @extendPenC@ causes a pendown.
--
extendPenC :: DPoint2 -> DVec2 -> DVec2 -> DVec2 -> ActivePen -> ActivePen
extendPenC pt v1 v2 v3 PEN_UP           = PEN_DOWN pt (curve1 v1 v2 v3)
extendPenC _  v1 v2 v3 (PEN_DOWN p0 rp) = PEN_DOWN p0 (snocCurveTo rp v1 v2 v3)



-- | Extend the path with a curve, drawn by the pen.
-- 
pencurve :: InterpretUnit u 
        => Vec2 u -> Vec2 u -> Vec2 u -> GenPathSpec st u ()
pencurve v1 v2 v3 = GenPathSpec $ \ctx pt s -> 
   let sz  = dc_font_size ctx
       dv1 = normalizeF sz v1
       dv2 = normalizeF sz v2
       dv3 = normalizeF sz v3
       pen = extendPenC pt dv1 dv2 dv3 (st_active_pen s)
       w1  = PathW { w_rel_path = line1 dv1, w_trace = mempty }
   in ((), pt .+^ dv1, s { st_active_pen = pen }, w1)


-- | @moveby@ causes a pen up.
--
movebyImpl :: InterpretUnit u => Vec2 u -> GenPathSpec st u ()
movebyImpl v1 = GenPathSpec $ \ctx pt s -> 
    let sz      = dc_font_size ctx
        dv1     = normalizeF sz v1
        (_,wcp) = runImage (drawActivePen PATH_OPEN $ st_active_pen s) ctx
        w1      = PathW { w_rel_path = mempty, w_trace = wcp }
    in ((), pt .+^ dv1, s { st_active_pen = PEN_UP }, w1)


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
insertlImpl gf = GenPathSpec $ \ctx pt s ->
    let upt     = dinterpF (dc_font_size ctx) pt
        (a,wcp) = runLocImage gf ctx upt
        w1      = PathW { w_rel_path = mempty, w_trace = wcp }
    in (a, pt, s, w1)



vamp :: InterpretUnit u => Vamp u -> GenPathSpec st u ()
vamp (Vamp v1 conn) = GenPathSpec $ \ctx pt s ->
    let sz      = dc_font_size ctx
        dv1     = normalizeF sz v1
        (_,wcp) = runImage (drawActivePen PATH_OPEN $ st_active_pen s) ctx
        upt     = dinterpF sz pt
        (_,ccp) = runConnectorImage conn ctx upt (upt .+^ v1)
        w1      = PathW { w_rel_path = mempty, w_trace = wcp `mappend` ccp }
    in ((), pt .+^ dv1, s { st_active_pen = PEN_UP }, w1)


cycleSubPath :: DrawStyle -> GenPathSpec st u ()
cycleSubPath styl = GenPathSpec $ \ctx pt s ->
    let gf      = drawActivePen (PATH_CLOSED styl) $ st_active_pen s
        (_,wcp) = runImage gf ctx
        w1      = PathW { w_rel_path = mempty, w_trace = wcp }
    in ((), pt, s { st_active_pen = PEN_UP }, w1)


-- Design note 
--
-- Should pen changing be @local@ style vis the Reader monad or a 
-- state change with the State monad?
-- 
-- @local@ is more idiomatic within the context of Wumpus (and 
-- easier to implement), but @state change@ is probably more 
-- natural for Path building.
-- 
-- For the time being we go with local.
--


localPen :: DrawingContextF -> GenPathSpec st u a -> GenPathSpec st u a
localPen upd ma = GenPathSpec $ \ctx pt s ->
    let (_,wcp)      = runImage (drawActivePen PATH_OPEN $ st_active_pen s) ctx
        (a,p1,s1,w1) = getGenPathSpec ma (upd ctx) pt s
        w2           = let wcp2 = wcp `mappend` w_trace w1 
                       in w1 { w_trace = wcp2 }
    in (a, p1, s1 { st_active_pen = PEN_UP }, w2)

