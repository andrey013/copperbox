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
  , execPivot


  , tip 
  , line
  , curve
  , move

  , insert
  , penCtxUpdate
  , vamp
  , cycle

  , setIncline
  , pivot

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
      , current_incline   :: Radian
      , active_path       :: (Vec2 u, RelPath u)
      , pen_dc_modifier   :: DrawingContextF
      , pivot_position    :: Vec2 u
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



zeroSt :: Num u => St u
zeroSt = St { cumulative_disp   = V2 0 0 
            , cumulative_path   = mempty
            , current_incline   = 0
            , active_path       = (V2 0 0, mempty)
            , pen_dc_modifier   = id
            , pivot_position    = V2 0 0
            }


runBuildMonad :: (Floating u, InterpretUnit u)
              => RelBuild u a -> (a, St u, LocGraphic u)
runBuildMonad mf = post $ getRelBuild mf zeroSt
  where
    post (a,st,log) = (a, st, pen `oplus` ins)
      where
        (v1,sub_last) = active_path st
        cf            = pen_dc_modifier st
        log_last      = logSubPath PATH_OPEN cf v1 sub_last
        log2          = log `mappend` log_last
        (pen,ins)     = extractTrace emptyLocGraphic log2


-- | Note - runAbsBuild drops the monadic answer and returns the
-- constructed path and a trace of the inserts and sub-paths.
--
runRelBuild :: (Floating u, InterpretUnit u)
            => RelBuild u a -> (RelPath u, LocGraphic u)
runRelBuild mf = post $ runBuildMonad mf
  where
    post (_,st,gf) = (cumulative_path st,gf)


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


execPivot :: (Floating u, InterpretUnit u)
             => RelBuild u a -> LocGraphic u
execPivot mf = post $ runBuildMonad mf
  where
    post (_,st,gf) = let v1 = pivot_position st
                     in moveStart (displaceVec $ vreverse v1) gf




logSubPath :: InterpretUnit u 
           => PathEnd -> DrawingContextF -> Vec2 u -> RelPath u -> Log u 
logSubPath spe upd v1 subp 
    | R.null subp  = mempty
    | otherwise    = pen1 gf
  where
    drawF = if spe == PATH_OPEN then dcOpenPath else dcClosedPath STROKE
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

--
-- Note - is the @r@ prefix of @rlineto@ redundant considering
-- RelBuild can only support relative operations anyway.
--
-- @lineto@ or probably better just @line@ would be shorter and
-- the derivatives @hline@, @vline@ would inherit an obvious 
-- naming scheme.
--


line :: Floating u => Vec2 u -> RelBuild u ()
line v1 = extendPath (\_ acc -> snocLineTo acc v1) v1





curve :: Floating u => Vec2 u -> Vec2 u -> Vec2 u -> RelBuild u ()
curve v1 v2 v3 = extendPath (\_ acc -> snocCurveTo acc v1 v2 v3) v3




-- | 'rmoveto' is a pen up.
--
move :: (Floating u, InterpretUnit u) => Vec2 u -> RelBuild u ()
move v1 = 
    gets active_path            >>= \(v0,ans) -> 
    gets pen_dc_modifier        >>= \cf -> 
    tellSubOpen cf v0 ans       >>  sets_ upd 
  where
    upd   = (\s v0 i -> s { cumulative_disp = v0 ^+^ v1
                          , cumulative_path = snocLineTo i v1
                          , active_path     = (v0 ^+^ v1, mempty) })
              <*> cumulative_disp <*> cumulative_path





insert :: Num u => LocGraphic u -> RelBuild u ()
insert gf = gets cumulative_disp >>= \v -> 
            tellInsert (moveStart (displaceVec v) gf)


penCtxUpdate :: (Floating u, InterpretUnit u) 
             => DrawingContextF -> RelBuild u ()
penCtxUpdate cf = move (V2 0 0) >> sets_ upd
  where
    upd = (\s f -> s { pen_dc_modifier = cf . f }) <*> pen_dc_modifier




-- Note - vamps should be a data type then we can have libraries 
-- of them.

vamp :: (Floating u, Ord u, Tolerance u, InterpretUnit u) 
     => Vamp u -> RelBuild u ()
vamp (Vamp vnext vstart upd relp path_end) = 
    gets cumulative_disp        >>= \v0 -> 
    gets pen_dc_modifier        >>= \cf ->
    move vnext                  >> drawF (upd . cf) (v0 ^+^ vstart) relp
  where
    drawF       = if path_end == PATH_OPEN then tellSubOpen else tellSubClosed
 

cycle :: (Floating u, InterpretUnit u) => RelBuild u ()
cycle = 
    gets cumulative_disp  >>= \v1 -> 
    gets pen_dc_modifier  >>= \cf ->
    gets active_path      >>= \(start,acc) -> 
    tellSubClosed cf start (snocLineTo acc start) >> 
    sets_ (\s -> s { active_path = (v1, mempty)})


setIncline :: Radian -> RelBuild u ()
setIncline ang = sets_ upd
  where
    upd = (\s -> s { current_incline = ang })


pivot :: RelBuild u ()
pivot = sets_ upd
  where
    upd = (\s i -> s { pivot_position = i }) <*> cumulative_disp


--------------------------------------------------------------------------------
-- Derived operators


pen_colour :: (Floating u, InterpretUnit u) 
           => RGBi -> RelBuild u ()
pen_colour rgb = penCtxUpdate (stroke_colour rgb)

pen_width  :: (Floating u, InterpretUnit u) 
           => Double -> RelBuild u ()
pen_width d = penCtxUpdate (set_line_width d)



hline :: Floating u => u -> RelBuild u ()
hline dx = line (hvec dx)

vline :: Floating u => u -> RelBuild u ()
vline dy = line (vvec dy)

aline :: Floating u => Radian -> u -> RelBuild u ()
aline ang u = line (avec ang u)

hmove :: (Floating u, InterpretUnit u) => u -> RelBuild u ()
hmove dx = move (hvec dx)

vmove :: (Floating u, InterpretUnit u) => u -> RelBuild u ()
vmove dy = move (vvec dy)


amove :: (Floating u, InterpretUnit u) => Radian -> u -> RelBuild u ()
amove ang u = move (avec ang u)


line_up :: Floating u => u -> RelBuild u ()
line_up u = line (vvec u)

line_down :: Floating u => u -> RelBuild u ()
line_down u = line (vvec $ negate u)

line_left :: Floating u => u -> RelBuild u ()
line_left u = line (hvec $ negate u)
 
line_right :: Floating u => u -> RelBuild u ()
line_right u = line (hvec u)

-- | Diagonal lines 

line_up_left :: Floating u => u -> RelBuild u ()
line_up_left u = line (vec (-u) u)

line_up_right :: Floating u => u -> RelBuild u ()
line_up_right u = line (vec u u)

line_down_left :: Floating u => u -> RelBuild u ()
line_down_left u = line (vec (-u) (-u))

line_down_right :: Floating u => u -> RelBuild u ()
line_down_right u = line (vec u (-u))


-- Cardinal lines

line_north :: Floating u => u -> RelBuild u ()
line_north = vline


line_south :: Floating u => u -> RelBuild u ()
line_south =  vline . negate

line_east :: Floating u => u -> RelBuild u ()
line_east = hline

line_west :: Floating u => u -> RelBuild u ()
line_west = hline . negate


line_northeast ::  Floating u => u -> RelBuild u ()
line_northeast = line . avec (0.25 * pi)

line_northwest ::  Floating u => u -> RelBuild u ()
line_northwest = line . avec (0.75 * pi)

line_southeast ::  Floating u => u -> RelBuild u ()
line_southeast = line . avec (1.75 * pi)

line_southwest ::  Floating u => u -> RelBuild u ()
line_southwest = line . avec (1.25 * pi)




move_up :: (Floating u, InterpretUnit u) => u -> RelBuild u ()
move_up u = move (vvec u)

move_down :: (Floating u, InterpretUnit u) => u -> RelBuild u ()
move_down u = move (vvec $ negate u)

move_left :: (Floating u, InterpretUnit u) => u -> RelBuild u ()
move_left u = move (hvec $ negate u)
 
move_right :: (Floating u, InterpretUnit u)  => u -> RelBuild u ()
move_right u = move (hvec u)



-- | Diagonal moves 

move_up_left :: (Floating u, InterpretUnit u) => u -> RelBuild u ()
move_up_left u = move (vec (-u) u)

move_up_right :: (Floating u, InterpretUnit u) => u -> RelBuild u ()
move_up_right u = move (vec u u)

move_down_left :: (Floating u, InterpretUnit u) => u -> RelBuild u ()
move_down_left u = move (vec (-u) (-u))

move_down_right :: (Floating u, InterpretUnit u) => u -> RelBuild u ()
move_down_right u = move (vec u (-u))


-- Cardinal moves

move_north :: (Floating u, InterpretUnit u) => u -> RelBuild u ()
move_north = vmove


move_south :: (Floating u, InterpretUnit u) => u -> RelBuild u ()
move_south =  vmove . negate

move_east :: (Floating u, InterpretUnit u) => u -> RelBuild u ()
move_east = hmove

move_west :: (Floating u, InterpretUnit u) => u -> RelBuild u ()
move_west = hmove . negate


move_northeast ::  (Floating u, InterpretUnit u) => u -> RelBuild u ()
move_northeast = move . avec (0.25 * pi)

move_northwest ::  (Floating u, InterpretUnit u) => u -> RelBuild u ()
move_northwest = move . avec (0.75 * pi)

move_southeast ::  (Floating u, InterpretUnit u) => u -> RelBuild u ()
move_southeast = move . avec (1.75 * pi)

move_southwest ::  (Floating u, InterpretUnit u) => u -> RelBuild u ()
move_southwest = move . avec (1.25 * pi)

