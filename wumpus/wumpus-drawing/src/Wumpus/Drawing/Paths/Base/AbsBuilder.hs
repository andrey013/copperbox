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
  , ctrlcurveto

  , insert
  , vamp

{-  , rlineto
  , hline
  , vline

  , bezierto

  , verticalHorizontal
  , horizontalVertical
-}
  ) where

import Wumpus.Drawing.Paths.Base.AbsPath
import qualified Wumpus.Drawing.Paths.Base.AbsPath as A
import Wumpus.Drawing.Paths.Base.BuildTrace
import qualified Wumpus.Drawing.Paths.Base.RelPath as R


import Wumpus.Basic.Kernel                      -- package: wumpus-basic

import Wumpus.Core                              -- package: wumpus-core

import Data.AffineSpace                         -- package: vector-space

import Control.Applicative hiding ( empty )
import Data.Monoid

import Prelude hiding ( log )

-- Note - connectors and paths are quite different things.
--
-- Connectors always know start and end points, a path is built 
-- from a start point.
--


-- State monad version is quite good - it ameliorates the problem
-- of joining to the end point of an empty path...

data St u = St
      { current_point     :: Point2 u 
      , cumulative_path   :: AbsPath u
      , active_path       :: (Point2 u, AbsPath u)
      }


type Log u  = BuildLog (Graphic u)


-- | Absolute Path builder monad.
--
newtype AbsBuild u a = AbsBuild { 
          getAbsBuild :: St u -> Log u -> (a, St u, Log u) }


--------------------------------------------------------------------------------
-- instances



instance Functor (AbsBuild u) where
  fmap f mf = AbsBuild $ \s0 t0 -> let (a,s1,t1) = getAbsBuild mf s0 t0
                                   in (f a, s1, t1)


instance Applicative (AbsBuild u) where
  pure a    = AbsBuild $ \s0 t0 -> (a,s0,t0)
  mf <*> ma = AbsBuild $ \s0 t0 -> let (f,s1,t1) = getAbsBuild mf s0 t0
                                       (a,s2,t2) = getAbsBuild ma s1 t1
                                in (f a,s2,t2)

instance Monad (AbsBuild u) where
  return a  = AbsBuild $ \s0 t0 -> (a,s0,t0)
  m >>= k   = AbsBuild $ \s0 t0 -> let (a,s1,t1) = getAbsBuild m s0 t0
                                   in (getAbsBuild . k) a s1 t1





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
runAbsBuild pt mf = post $ getAbsBuild mf (initSt pt) mempty
  where
    post (_,st,log) = let sub_last  = snd $ active_path st
                          empty_gfx = emptyLocGraphic `at` pt
                          log2      = logSubPath SPE_Open id sub_last log
                          (pen,ins) = extractTrace empty_gfx log2
                      in (cumulative_path st, pen `oplus` ins)


execAbsBuild :: (Floating u, InterpretUnit u)
             => Point2 u -> AbsBuild u a -> Graphic u
execAbsBuild pt mf = snd $ runAbsBuild pt mf

evalAbsBuild :: (Floating u, InterpretUnit u)
             => Point2 u -> AbsBuild u a -> AbsPath u
evalAbsBuild pt mf = fst $ runAbsBuild pt mf



logSubPath :: InterpretUnit u 
           => SubPathEnd -> DrawingContextF -> AbsPath u -> Log u -> Log u 
logSubPath spe upd subp log 
    | A.null subp  = log
    | otherwise    = addPen (toPrimPath subp >>= localize upd . drawF) log
  where
    drawF = if spe == SPE_Closed then closedStroke else openStroke



tellSubClosed :: InterpretUnit u 
              => DrawingContextF -> AbsPath u -> AbsBuild u ()
tellSubClosed upd subp = 
    AbsBuild $ \s0 t0 -> ((), s0, logSubPath SPE_Closed upd subp t0)

tellSubOpen :: InterpretUnit u 
            => DrawingContextF -> AbsPath u -> AbsBuild u ()
tellSubOpen upd subp = 
    AbsBuild $ \s0 t0 -> ((), s0, logSubPath SPE_Open upd subp t0)


tellInsert :: Graphic u -> AbsBuild u ()
tellInsert g1 = 
    AbsBuild $ \s0 t0 -> ((),s0, addInsert g1 t0)


sets_   :: (St u -> St u) -> AbsBuild u ()
sets_ f = AbsBuild $ \s0 t0 -> ((), f s0, t0)


gets    :: (St u -> a) -> AbsBuild u a
gets f  = AbsBuild $ \s0 t0 -> (f s0, s0, t0)



--------------------------------------------------------------------------------
-- operations

tip :: AbsBuild u (Point2 u)
tip = gets current_point




lineto :: Floating u => (u,u) -> AbsBuild u ()
lineto (x,y) = sets_ upd
  where
    pt  = P2 x y
    upd = (\s i j -> s { current_point    = pt
                       , cumulative_path  = i `snocLineTo` pt
                       , active_path      = bimapR (`snocLineTo` pt) j })
            <*> cumulative_path <*> active_path
   



curveto :: (Floating u, Ord u, Tolerance u)
        => (u,u) -> (u,u) -> (u,u) -> AbsBuild u ()
curveto (x1,y1) (x2,y2) (x3,y3) = sets_ upd
  where
    p1  = P2 x1 y1
    p2  = P2 x2 y2
    p3  = P2 x3 y3
    ext = (\abp -> snocCurveTo abp p1 p2 p3)
    upd = (\s i j -> s { current_point    = p3
                       , cumulative_path  = ext i
                       , active_path      = bimapR ext j }) 
            <*> cumulative_path <*> active_path



ctrlcurveto :: (Floating u, Ord u, Tolerance u) 
            => Radian -> Radian -> (u,u)-> AbsBuild u ()
ctrlcurveto cin cout (x1,y1) = sets_ upd
  where
    p1  = P2 x1 y1
    upd = (\s i j k -> let suffix = controlCurve i cin cout p1
                       in s { current_point    = p1
                            , cumulative_path  = j `append` suffix
                            , active_path      = bimapR (`append` suffix) k }) 
            <*> current_point <*> cumulative_path <*> active_path


   
-- | 'moveto' is a pen up.
--
moveto :: (Floating u, Ord u, Tolerance u, InterpretUnit u) 
       => (u,u) -> AbsBuild u ()
moveto (x,y) = 
    gets active_path >>= \(_,ans) -> tellSubOpen id ans >> sets_ upd 
  where
    pnext = P2 x y
    upd   = (\s i -> s { current_point   = pnext
                       , cumulative_path = i `snocLineTo` pnext
                       , active_path     = (pnext, empty pnext) }) 
              <*> cumulative_path



insert :: Num u => LocGraphic u -> AbsBuild u ()
insert gf = gets current_point >>= \pt -> tellInsert (gf `at` pt)


vamp :: (Floating u, Ord u, Tolerance u, InterpretUnit u) 
     => Vec2 u -> DrawingContextF -> R.RelPath u -> AbsBuild u ()
vamp vnext upd relp = 
    gets current_point                          >>= \p0 -> 
    let (P2 x y) = p0 .+^ vnext in moveto (x,y) >>
    let pvamp = R.toAbsPath p0 relp in tellSubOpen upd pvamp


{-
-- Running the path is (probably) agnostic to the DrawingCtx.
--
runPath :: Floating u => Point2 u -> AbsBuild u a -> (a, AbsPath u)
runPath start mf = 
    let (a,s') = getAbsBuild mf s in (a, post $ toListH $ path_acc s')
  where
    s = St { current_point = start
           , path_acc      = emptyH
           }
    post []     = line start start
    post (x:xs) = foldl' append x xs  

execPath :: Floating u => Point2 u -> AbsBuild u a -> AbsPath u
execPath start mf = snd $ runPath start mf

snocline :: Floating u => Vec2 u -> AbsBuild u ()
snocline v = AbsBuild $ \(St pt ac) -> let ep = pt .+^ v 
                                    in ((), St ep (ac `snocH` line pt ep))


tip :: AbsBuild u (Point2 u)
tip = AbsBuild $ \s -> (current_point s,s)


lineto :: Floating u => Point2 u -> AbsBuild u ()
lineto pt = AbsBuild $ \(St p0 ac) -> ((), St pt (ac `snocH` line p0 pt))

rlineto :: Floating u => Vec2 u -> AbsBuild u ()
rlineto (V2 dx dy) = tip >>= \(P2 x y) -> lineto (P2 (x+dx) (y+dy))


hline :: Floating u => u -> AbsBuild u ()
hline len = snocline (hvec len) 

vline :: Floating u => u -> AbsBuild u ()
vline len = snocline (vvec len) 



bezierto :: (Floating u, Ord u, Tolerance u) 
         => Point2 u -> Point2 u -> Point2 u -> AbsBuild u ()
bezierto c1 c2 ep = AbsBuild $ \(St p0 ac) -> 
    ((), St ep (ac `snocH` curve p0 c1 c2 ep))





--


curveto :: (Floating u, Ord u, Tolerance u) 
        => Radian -> Radian -> Point2 u -> AbsBuild u ()
curveto cin cout end = AbsBuild $ \(St p0 ac) -> 
    let seg  = curveByAngles p0 cin cout end 
        ac1  = ac `snocH` seg
        end1 = tipR seg
    in ((), St end1 ac1) 




verticalHorizontal :: Floating u => Point2 u -> AbsBuild u ()
verticalHorizontal (P2 x y) = 
    tip >>= \(P2 x0 _) -> lineto (P2 x0 y) >> lineto (P2 x y)

horizontalVertical :: Floating u => Point2 u -> AbsBuild u ()
horizontalVertical (P2 x y) = 
    tip >>= \(P2 _ y0) -> lineto (P2 x y0) >> lineto (P2 x y)


-}