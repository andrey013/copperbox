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

  , line
  , curve
  , move

  , relline
  , relcurve
  , relmove

  , rellineParallel
  , rellinePerpendicular

  , relmoveParallel
  , relmovePerpendicular



  , ctrlcurve

  , insert
  , vamp
  , cycle
  , setIncline

  -- * Derived operators
  , pen_colour
  , pen_width

  , hline
  , vline
  , aline

  , hmove
  , vmove
  , amove

  , line_up
  , line_down
  , line_left
  , line_right

  , line_up_left
  , line_up_right
  , line_down_left
  , line_down_right

  , line_north
  , line_south
  , line_east
  , line_west
  , line_northeast
  , line_northwest
  , line_southeast
  , line_southwest

  , move_up
  , move_down
  , move_left
  , move_right

  , move_up_left
  , move_up_right
  , move_down_left
  , move_down_right

  , move_north
  , move_south
  , move_east
  , move_west
  , move_northeast
  , move_northwest
  , move_southeast
  , move_southwest


  ) where

import Wumpus.Drawing.Paths.Base.AbsPath
import qualified Wumpus.Drawing.Paths.Base.AbsPath as A
import Wumpus.Drawing.Paths.Base.BuildCommon
import qualified Wumpus.Drawing.Paths.Base.RelPath as R


import Wumpus.Basic.Geometry ( half_pi )        -- package: wumpus-basic
import Wumpus.Basic.Kernel

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
      , current_incline   :: Radian
      , cumulative_path   :: AbsPath u
      , active_path       :: (Point2 u, AbsPath u)
      , pen_dc_modifier   :: DrawingContextF
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
               , current_incline   = 0
               , cumulative_path   = empty pt
               , active_path       = (pt, empty pt)
               , pen_dc_modifier   = id
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
                          cf        = pen_dc_modifier st
                          log_last  = logSubPath PATH_OPEN cf sub_last
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

line :: Floating u => Point2 u -> AbsBuild u ()
line p1 = extendPath (\_ acc -> acc `snocLineTo` p1) p1



curve :: (Floating u, Ord u, Tolerance u)
        => Point2 u -> Point2 u -> Point2 u -> AbsBuild u ()
curve p1 p2 p3 = extendPath (\_ acc -> snocCurveTo acc p1 p2 p3) p3



   
-- | 'move' is a pen up.
--
move :: (Floating u, Ord u, Tolerance u, InterpretUnit u) 
       => Point2 u -> AbsBuild u ()
move p1 = 
    gets active_path            >>= \(_,ans) -> 
    gets pen_dc_modifier        >>= \cf -> 
    tellSubOpen cf ans          >> sets_ upd 
  where
    upd   = (\s i -> s { current_point   = p1
                       , cumulative_path = i `snocLineTo` p1
                       , active_path     = (p1, empty p1) }) 
              <*> cumulative_path


relline :: Floating u => Vec2 u -> AbsBuild u ()
relline v1 = gets current_point >>= \pt -> line (pt .+^ v1)


relcurve :: (Floating u, Ord u, Tolerance u)
         => Vec2 u -> Vec2 u -> Vec2 u -> AbsBuild u ()
relcurve v1 v2 v3 = 
    gets current_point >>= \pt -> 
    curve (pt .+^ v1) (pt .+^ v1 ^+^ v2) (pt .+^ v1 ^+^ v2 ^+^ v3)


relmove :: (Floating u, Ord u, Tolerance u, InterpretUnit u) 
        => Vec2 u -> AbsBuild u ()
relmove v1 = gets current_point >>= \pt -> move (pt .+^ v1)



rellineParallel :: Floating u => u -> AbsBuild u ()
rellineParallel u = gets current_incline >>= \ang -> relline (avec ang u)

rellinePerpendicular :: Floating u => u -> AbsBuild u ()
rellinePerpendicular u = 
    gets current_incline >>= \ang -> relline (avec (fn ang) u)
  where
    fn = circularModulo . (+ half_pi)

relmoveParallel :: (Floating u, Ord u, Tolerance u, InterpretUnit u) 
                => u -> AbsBuild u ()
relmoveParallel u = gets current_incline >>= \ang -> relmove (avec ang u)


relmovePerpendicular :: (Floating u, Ord u, Tolerance u, InterpretUnit u) 
                     => u -> AbsBuild u ()
relmovePerpendicular u = 
    gets current_incline >>= \ang -> relmove (avec (fn ang) u)
  where
    fn = circularModulo . (+ half_pi)



ctrlcurve :: (Floating u, Ord u, Tolerance u) 
          => Radian -> Radian -> Point2 u -> AbsBuild u ()
ctrlcurve cin cout p1 = 
    extendPath (\p0 acc -> acc `append` controlCurve p0 cin cout p1) p1



insert :: Num u => LocGraphic u -> AbsBuild u ()
insert gf = gets current_point >>= \pt -> tellInsert (gf `at` pt)


penCtxUpdate :: (Floating u, Ord u, Tolerance u, InterpretUnit u) 
             => DrawingContextF -> AbsBuild u ()
penCtxUpdate cf = relmove (V2 0 0) >> sets_ upd
  where
    upd = (\s f -> s { pen_dc_modifier = cf . f })
            <*> pen_dc_modifier



-- Note - vamps should be a data type then we can have libraries 
-- of them.

vamp :: (Floating u, Ord u, Tolerance u, InterpretUnit u) 
     => Vamp u -> AbsBuild u ()
vamp (Vamp vnext vstart upd relp path_end) = 
    gets current_point >>= \p0 -> 
    move (p0 .+^ vnext) >> drawF upd (R.toAbsPath (p0 .+^ vstart) relp)
  where
    drawF = if path_end == PATH_OPEN then tellSubOpen else tellSubClosed

cycle :: (Floating u, InterpretUnit u) => AbsBuild u ()
cycle = 
    gets current_point    >>= \pt -> 
    gets pen_dc_modifier  >>= \cf ->
    gets active_path      >>= \(start,acc) -> 
    tellSubClosed cf (acc `snocLineTo` start) >> 
    sets_ (\s -> s { active_path = (pt, empty pt)})


setIncline :: Radian -> AbsBuild u ()
setIncline ang = sets_ upd
  where
    upd = (\s -> s { current_incline = ang })


--------------------------------------------------------------------------------
-- Derived operators


pen_colour :: (Floating u, Ord u, Tolerance u, InterpretUnit u) 
           => RGBi -> AbsBuild u ()
pen_colour rgb = penCtxUpdate (stroke_colour rgb)

pen_width  :: (Floating u, Ord u, Tolerance u, InterpretUnit u) 
           => Double -> AbsBuild u ()
pen_width d = penCtxUpdate (set_line_width d)




hline :: Floating u => u -> AbsBuild u ()
hline dx = relline (hvec dx)

vline :: Floating u => u -> AbsBuild u ()
vline dy = relline (vvec dy)

aline :: Floating u => Radian -> u -> AbsBuild u ()
aline ang u = relline (avec ang u)

hmove :: (Floating u, Ord u, Tolerance u, InterpretUnit u) 
      => u -> AbsBuild u ()
hmove dx = relmove (hvec dx)

vmove :: (Floating u, Ord u, Tolerance u, InterpretUnit u) 
      => u -> AbsBuild u ()
vmove dy = relmove (vvec dy)


amove :: (Floating u, Ord u, Tolerance u, InterpretUnit u) 
      => Radian -> u -> AbsBuild u ()
amove ang u = relmove (avec ang u)



line_up :: Floating u => u -> AbsBuild u ()
line_up u = relline (vvec u)

line_down :: Floating u => u -> AbsBuild u ()
line_down u = relline (vvec $ negate u)

line_left :: Floating u => u -> AbsBuild u ()
line_left u = relline (hvec $ negate u)
 
line_right :: Floating u => u -> AbsBuild u ()
line_right u = relline (hvec u)

-- | Diagonal lines 

line_up_left :: Floating u => u -> AbsBuild u ()
line_up_left u = relline (vec (-u) u)

line_up_right :: Floating u => u -> AbsBuild u ()
line_up_right u = relline (vec u u)

line_down_left :: Floating u => u -> AbsBuild u ()
line_down_left u = relline (vec (-u) (-u))

line_down_right :: Floating u => u -> AbsBuild u ()
line_down_right u = relline (vec u (-u))


-- Cardinal lines

line_north :: Floating u => u -> AbsBuild u ()
line_north = vline


line_south :: Floating u => u -> AbsBuild u ()
line_south =  vline . negate

line_east :: Floating u => u -> AbsBuild u ()
line_east = hline

line_west :: Floating u => u -> AbsBuild u ()
line_west = hline . negate


line_northeast ::  Floating u => u -> AbsBuild u ()
line_northeast = relline . avec (0.25 * pi)

line_northwest ::  Floating u => u -> AbsBuild u ()
line_northwest = relline . avec (0.75 * pi)

line_southeast ::  Floating u => u -> AbsBuild u ()
line_southeast = relline . avec (1.75 * pi)

line_southwest ::  Floating u => u -> AbsBuild u ()
line_southwest = relline . avec (1.25 * pi)




move_up :: (Floating u, Ord u, Tolerance u, InterpretUnit u) 
        => u -> AbsBuild u ()
move_up u = relmove (vvec u)

move_down :: (Floating u, Ord u, Tolerance u, InterpretUnit u) 
          => u -> AbsBuild u ()
move_down u = relmove (vvec $ negate u)

move_left :: (Floating u, Ord u, Tolerance u, InterpretUnit u) 
          => u -> AbsBuild u ()
move_left u = relmove (hvec $ negate u)
 
move_right :: (Floating u, Ord u, Tolerance u, InterpretUnit u)  
           => u -> AbsBuild u ()
move_right u = relmove (hvec u)



-- | Diagonal moves 

move_up_left :: (Floating u, Ord u, Tolerance u, InterpretUnit u) 
             => u -> AbsBuild u ()
move_up_left u = relmove (vec (-u) u)

move_up_right :: (Floating u, Ord u, Tolerance u, InterpretUnit u) 
              => u -> AbsBuild u ()
move_up_right u = relmove (vec u u)

move_down_left :: (Floating u, Ord u, Tolerance u, InterpretUnit u) 
               => u -> AbsBuild u ()
move_down_left u = relmove (vec (-u) (-u))

move_down_right :: (Floating u, Ord u, Tolerance u, InterpretUnit u) 
                => u -> AbsBuild u ()
move_down_right u = relmove (vec u (-u))


-- Cardinal moves

move_north :: (Floating u, Ord u, Tolerance u, InterpretUnit u) 
           => u -> AbsBuild u ()
move_north = vmove


move_south :: (Floating u, Ord u, Tolerance u, InterpretUnit u) 
           => u -> AbsBuild u ()
move_south =  vmove . negate

move_east :: (Floating u, Ord u, Tolerance u, InterpretUnit u) 
          => u -> AbsBuild u ()
move_east = hmove

move_west :: (Floating u, Ord u, Tolerance u, InterpretUnit u) 
          => u -> AbsBuild u ()
move_west = hmove . negate


move_northeast :: (Floating u, Ord u, Tolerance u, InterpretUnit u) 
               => u -> AbsBuild u ()
move_northeast = relmove . avec (0.25 * pi)

move_northwest :: (Floating u, Ord u, Tolerance u, InterpretUnit u) 
               => u -> AbsBuild u ()
move_northwest = relmove . avec (0.75 * pi)

move_southeast :: (Floating u, Ord u, Tolerance u, InterpretUnit u) 
               => u -> AbsBuild u ()
move_southeast = relmove . avec (1.75 * pi)

move_southwest :: (Floating u, Ord u, Tolerance u, InterpretUnit u) 
               => u -> AbsBuild u ()
move_southwest = relmove . avec (1.25 * pi)



