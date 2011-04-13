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
    PathSpec
  , Action
  , fillPathSpec
  , strokePathSpec
 
  , moveto
  , lineto
  , curveto
  , insert
  , vamp

  ) where

import Wumpus.Drawing.Paths.Base.RelPath
import Wumpus.Drawing.Paths.Base.AbsPath ( AbsPath )
import qualified Wumpus.Drawing.Paths.Base.AbsPath as Abs


import Wumpus.Basic.Kernel                      -- package: wumpus-basic
import Wumpus.Basic.Utils.HList

import Wumpus.Core                              -- package: wumpus-core


import Data.VectorSpace                         -- package: vector-space

import Control.Applicative
import Data.Monoid
import Prelude hiding ( null )

-- PathSpec is a non-empty list of segments and DrawingContext 
-- update functions.
--
type PathSpec u = [(DrawingContextF, [Action u])]

-- Note - with TikZ draw (stroke) does not close the path.

-- Note - Action is wrong. Use monadic operations and (>>) like 
-- MonadicConstruction.
--

data Action u = Line    (Vec2 u)
              | Move    (Vec2 u)
              | Curve   (Vec2 u)  (Vec2 u)  (Vec2 u)
              | Insert  (LocGraphic u)
              | Vamp    (Vec2 u)  DrawingContextF  (RelPath u)



data St u = St 
      { cumulative_disp   :: Vec2 u
      , cumulative_path   :: RelPath u
      , active_path       :: (Vec2 u, RelPath u)
      }


data Log u  = Log { insert_trace    :: H (LocGraphic u)
                  , pen_trace       :: H (LocGraphic u)
                  }
            | NoLog



 
-- Don\'t want to write pen trace along with the insert commands 
-- as some renderings (fill) should ignore the the pen trace.


-- | Evaluation is two States - a vector for cummulative 
-- displacement and a cummulative path - plus one Writer - a trace 
-- of TikZ-like @insert@ commands.
--
data RelBuild u a = RelBuild { getRelBuild :: St u -> (a, St u, Log u) }


--------------------------------------------------------------------------------
-- instances


instance Monoid (Log a) where
  mempty                        = NoLog
  NoLog     `mappend` b         = b
  a         `mappend` NoLog     = a
  Log li lp `mappend` Log ri rp = Log (li `appendH` ri) (lp `appendH` rp)



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

-- | Cumulative vector * commulative path * path_img * insert_img
--
type SegmentAns u = (Vec2 u, RelPath u, LocGraphic u, LocGraphic u)

-- Note - active pen needs flushing.
--
execRelBuild :: (Floating u, Ord u, Tolerance u, InterpretUnit u) 
          => Vec2 u -> RelBuild u a -> SegmentAns u
execRelBuild v0 mf = post $ getRelBuild mf (initSt v0)
  where
    post (_,st,w) = let last_path = uncurry penGraphic $ active_path st
                        path_img  = penTrace w `oplus` last_path
                        ins_img   = insertTrace w
                        path_ans  = cumulative_path st 
                        v_end     = cumulative_disp st
                    in (v_end, path_ans, path_img, ins_img)



penTrace :: InterpretUnit u => Log u -> LocGraphic u
penTrace NoLog                   = emptyLocGraphic
penTrace (Log { pen_trace = hl}) = altconcat emptyLocGraphic $ toListH hl

insertTrace :: InterpretUnit u => Log u -> LocGraphic u
insertTrace NoLog                      = emptyLocGraphic
insertTrace (Log { insert_trace = hl}) = altconcat emptyLocGraphic $ toListH hl



initSt :: Num u => Vec2 u -> St u
initSt v = St { cumulative_disp   = v
              , cumulative_path   = emptyRelPath
              , active_path       = (v, emptyRelPath)
              }


      

-- sets :: (St u -> (a, St u)) -> RelBuild u a
-- sets f = RelBuild $ \s0  -> let (a,s1) = f s0 in (a, s1, mempty)

sets_ :: (St u -> St u) -> RelBuild u ()
sets_ f = RelBuild $ \s0  -> ((), f s0, mempty)

-- get :: RelBuild u (St u)
-- get = RelBuild $ \s0 -> (s0, s0, mempty)

gets :: (St u -> a) -> RelBuild u a
gets f = RelBuild $ \s0 -> (f s0, s0, mempty)

tellInsertImage :: LocGraphic u -> RelBuild u ()
tellInsertImage g = RelBuild $ \s0 -> ((), s0, log1)
   where
     log1 = Log { insert_trace    = wrapH g
                , pen_trace       = emptyH  }


    
tellVPath :: (Floating u, Ord u, Tolerance u, InterpretUnit u) 
          => (Vec2 u, RelPath u) -> DrawingContextF -> RelBuild u ()
tellVPath (v,rp) upd | null rp   = return () 
                     | otherwise = RelBuild $ \s0 -> ((), s0, log1)
  where
    log1 = Log { insert_trace = emptyH
               , pen_trace    = wrapH $ localize upd $ penGraphic v rp }



perform :: (Floating u, Ord u, Tolerance u, InterpretUnit u) 
        => Vec2 u -> [Action u] -> SegmentAns u
perform v []     = (v, emptyRelPath, emptyLocGraphic, emptyLocGraphic)
perform v (x:xs) = execRelBuild v (go x xs)
  where
    go a []     = step1 a 
    go a (b:bs) = step1 a >> go b bs 
    
    step1 (Line v1)             = interpLine v1
    step1 (Move v1)             = interpMove v1
    step1 (Curve v1 v2 v3)      = interpCurve v1 v2 v3
    step1 (Insert gf)           = interpInsert gf
    step1 (Vamp v1 upd rp)      = interpVamp v1 upd rp


penGraphic :: (Floating u, Ord u, Tolerance u, InterpretUnit u) 
           => Vec2 u -> RelPath u -> LocGraphic u
penGraphic v p = locGraphic_ $ moveStart (displaceVec v) $ drawPath p


interpLine :: Num u => Vec2 u -> RelBuild u () 
interpLine v = sets_ upd 
  where
   upd = (\s i j k -> s { cumulative_disp = i ^+^ v
                        , cumulative_path = j `append` lineTo v 
                        , active_path     = fn k (lineTo v) }) 
           <*> cumulative_disp <*> cumulative_path <*> active_path
   
   fn (sv,p1) p2 = (sv,p1 `append` p2)

interpInsert :: Num u => LocGraphic u -> RelBuild u ()
interpInsert gf = gets cumulative_disp >>= \v -> 
                  tellInsertImage (moveStart (displaceVec v) gf)




interpCurve :: Num u => Vec2 u -> Vec2 u -> Vec2 u -> RelBuild u () 
interpCurve v1 v2 v3 = sets_ upd 
  where
   upd = (\s i j k -> let curve1 = curveTo v1 v2 v3
                      in s { cumulative_disp = i ^+^ v1 ^+^ v2 ^+^ v3
                           , cumulative_path = j `append` curve1
                          , active_path     = fn k curve1 }) 
           <*> cumulative_disp <*> cumulative_path <*> active_path
   
   fn (sv,p1) p2 = (sv,p1 `append` p2)
   



interpMove :: (Floating u, Ord u, Tolerance u, InterpretUnit u) 
           => Vec2 u -> RelBuild u ()
interpMove vnext = 
    gets active_path >>= \ans -> tellVPath ans id >> sets_ upd 
  where
   upd = (\s i j -> let vcurrent = i ^+^ vnext
                    in s { cumulative_disp = vcurrent
                         , cumulative_path = j `append` lineTo vnext
                         , active_path     = (vcurrent, emptyRelPath) }) 
           <*> cumulative_disp <*> cumulative_path

   

interpVamp :: (Floating u, Ord u, Tolerance u, InterpretUnit u) 
           => Vec2 u -> DrawingContextF -> RelPath u -> RelBuild u ()
interpVamp vnext upd rp = 
    gets cumulative_disp >>= \v0 -> interpMove vnext >> tellVPath (v0,rp) upd
    



execPathSpec :: (Floating u, Ord u, Tolerance u, InterpretUnit u) 
             => PathSpec u -> (RelPath u, LocGraphic u, LocGraphic u)
execPathSpec []     = (emptyRelPath, emptyLocGraphic, emptyLocGraphic)
execPathSpec (a:as) = go1 (V2 0 0) a as
  where
    go1 v (upd,cmds) xs                   = 
        let (v1,path,gfp,gfi) = perform v cmds
        in go2 v1 (path, localize upd gfp, gfi) xs

    go2 _ acc            []               = acc
    go2 v (p0,gfp0,gfi0) ((upd,cmds):xs)  = 
        let (v1,p1,gfp1,gfi1) = perform v cmds
            gpath             = gfp0 `oplus` localize upd gfp1
            gins              = gfi0 `oplus` gfi1
        in go2 v1 (p0 `append` p1, gpath, gins) xs
                                        

fillPathSpec :: (Floating u, Ord u, Tolerance u, InterpretUnit u) 
             => PathSpec u -> LocImage u (AbsPath u)
fillPathSpec spec = promoteR1 $ \start -> 
    let (rp,_,deco) = execPathSpec spec
        ap          = toAbsPath start rp
    in Abs.toPrimPath ap >>= \pp -> 
       fmap (replaceAns ap) $ decorateR0 (filledPath pp) (deco `at` start)


strokePathSpec :: (Floating u, Ord u, Tolerance u, InterpretUnit u) 
             => PathSpec u -> LocImage u (AbsPath u)
strokePathSpec spec = promoteR1 $ \start -> 
    let (rp,img,deco) = execPathSpec spec
        ap            = toAbsPath start rp
    in fmap (replaceAns ap) $ decorateR0 (img `at` start) (deco `at` start)


drawPath :: (Floating u, Ord u, Tolerance u, InterpretUnit u) 
         => RelPath u -> LocImage u (AbsPath u)
drawPath rp = promoteR1 $ \start -> 
    let ap = toAbsPath start rp
    in fmap (replaceAns ap) $ Abs.toPrimPath ap >>= openStroke




    
--------------------------------------------------------------------------------
-- user commands


lineto :: (u,u) -> Action u
lineto (x,y) = Line $ V2 x y


moveto :: (u,u) -> Action u
moveto (x,y) = Move (V2 x y)

curveto :: (u,u) -> (u,u) -> (u,u) -> Action u
curveto (x1,y1) (x2,y2) (x3,y3) = Curve (V2 x1 y1) (V2 x2 y2) (V2 x3 y3)

insert :: LocGraphic u -> Action u
insert = Insert

vamp :: (u,u) -> DrawingContextF -> RelPath u -> Action u
vamp (x,y) upd rp = Vamp (V2 x y) upd rp