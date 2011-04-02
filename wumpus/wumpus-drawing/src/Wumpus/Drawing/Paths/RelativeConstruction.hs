{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Drawing.Paths.RelativeConstruction
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Build relative paths.
--
-- \*\* WARNING \*\* this module is an experiment, and may 
-- change significantly or even be dropped from future revisions.
-- 
--------------------------------------------------------------------------------

module Wumpus.Drawing.Paths.RelativeConstruction
  ( 
    PathSpec
  , Action
  , fillPathSpec
  , strokePathSpec
 
  , move
  , insert
  , pen_up
  , pen_down

  

  ) where

import Wumpus.Drawing.Paths.Relative
import Wumpus.Drawing.Paths.Absolute ( AbsPath )
import qualified Wumpus.Drawing.Paths.Absolute as Abs


import Wumpus.Basic.Kernel                      -- package: wumpus-basic
import Wumpus.Basic.Utils.HList

import Wumpus.Core                              -- package: wumpus-core


import Data.VectorSpace                         -- package: vector-space

import Control.Applicative
import Data.Monoid


-- PathSpec is a non-empty list of segments and DrawingContext 
-- update functions.
--
type PathSpec u = [(DrawingContextF, [Action u])]

-- Note - with TikZ draw (stroke) does not close the path.

data Action u = Move (Vec2 u)
              | Insert (LocGraphic u)
              | PenUp
              | PenDown

data ActivePen path = PenStateUp | PenStateDown path

type RelPen u = ActivePen (RelPath u)

continuePath :: RelPath u -> RelPen u -> RelPen u
continuePath _  PenStateUp         = PenStateUp
continuePath s1 (PenStateDown acc) = PenStateDown $ acc `append` s1


continuePen :: RelPath u -> (Vec2 u, RelPen u) -> (Vec2 u, RelPen u)
continuePen s1 (v,pen) = (v, continuePath s1 pen)


data St u = St 
      { cumulative_disp   :: Vec2 u
      , cumulative_path   :: RelPath u
      , active_pen        :: (Vec2 u, RelPen u)
      }

data Log u = Log { insert_trace    :: H (LocGraphic u)
                 , pen_trace       :: H (LocGraphic u)
                 }
           | NoLog

instance Monoid (Log u) where
  mempty                = NoLog
  NoLog `mappend` b     = b
  a     `mappend` NoLog = a
  a     `mappend` b     = Log (insert_trace a `appendH` insert_trace b)
                              (pen_trace a `appendH` pen_trace b)

 
-- Don\'t want to write pen trace along with the insert commands 
-- as some renderings (fill) should ignore the the pen trace.


-- | Evaluation is two States - a vector for cummulative 
-- displacement and a cummulative path - plus one Writer - a trace 
-- of TikZ-like @insert@ commands.
--
data EvalM u a = EvalM { getEvalM :: St u -> (a, St u, Log u) }


instance Functor (EvalM u) where
  fmap f mf = EvalM $ \s0 -> let (a, s1, w) = getEvalM mf s0
                             in (f a, s1, w)


instance Applicative (EvalM u) where
  pure a    = EvalM $ \st0   -> (a, st0, mempty)
  mf <*> ma = EvalM $ \st0 -> 
                let (f,st1,w1) = getEvalM mf st0
                    (a,st2,w2) = getEvalM ma st1
                in (f a, st2, w1 `mappend` w2)

instance Monad (EvalM u) where
  return a  = EvalM $ \st0   -> (a, st0, mempty)
  ma >>= k  = EvalM $ \st0 -> 
                let (a,st1,w1) = getEvalM ma st0
                    (b,st2,w2) = (getEvalM . k) a st1
                in (b, st2, w1 `mappend` w2)

-- | Cumulative vector * commulative path * path_img * insert_img
--
type SegmentAns u = (Vec2 u, RelPath u, LocGraphic u, LocGraphic u)

-- Note - active pen needs flushing.
--
execEvalM :: (Floating u, Ord u, LengthTolerance u, InterpretUnit u) 
          => Vec2 u -> EvalM u a -> SegmentAns u
execEvalM v0 mf = post $ getEvalM mf (initSt v0)
  where
    post (_,st,w) = let path_img = penTrace w
                        ins_img  = insertTrace w
                        path_ans = cumulative_path st 
                        v_end    = cumulative_disp st
                    in case active_pen st of
                         (_, PenStateUp) -> (v_end, path_ans, path_img, ins_img)
                         
                         (v, PenStateDown p) -> let g1 = penGraphic v p 
                                                in ( v_end
                                                   , path_ans 
                                                   , path_img `oplus` g1
                                                   , ins_img )
-- warning wrong v...

penTrace :: InterpretUnit u => Log u -> LocGraphic u
penTrace NoLog                   = emptyLocGraphic
penTrace (Log { pen_trace = hl}) = altconcat emptyLocGraphic $ toListH hl

insertTrace :: InterpretUnit u => Log u -> LocGraphic u
insertTrace NoLog                      = emptyLocGraphic
insertTrace (Log { insert_trace = hl}) = altconcat emptyLocGraphic $ toListH hl



initSt :: Num u => Vec2 u -> St u
initSt v = St { cumulative_disp   = v
              , cumulative_path   = emptyRelPath
              , active_pen        = (v, PenStateDown emptyRelPath)
              }


perform :: (Floating u, Ord u, LengthTolerance u, InterpretUnit u) 
        => Vec2 u -> [Action u] -> SegmentAns u
perform v []     = (v, emptyRelPath, emptyLocGraphic, emptyLocGraphic)
perform v (x:xs) = execEvalM v (go x xs)
  where
    go a []     = step1 a 
    go a (b:bs) = step1 a >> go b bs 
    
    step1 (Move v1)   = interpMove v1
    step1 (Insert gf) = interpInsert gf
    step1 PenUp       = interpPenUp
    step1 PenDown     = interpPenDown


penGraphic :: (Floating u, Ord u, LengthTolerance u, InterpretUnit u) 
           => Vec2 u -> RelPath u -> LocGraphic u
penGraphic v p = ignoreAns $ moveStart (displaceVec v) $ drawPath p 
      

-- sets :: (St u -> (a, St u)) -> EvalM u a
-- sets f = EvalM $ \s0  -> let (a,s1) = f s0 in (a, s1, mempty)

sets_ :: (St u -> St u) -> EvalM u ()
sets_ f = EvalM $ \s0  -> ((), f s0, mempty)

-- get :: EvalM u (St u)
-- get = EvalM $ \s0 -> (s0, s0, mempty)

gets :: (St u -> a) -> EvalM u a
gets f = EvalM $ \s0 -> (f s0, s0, mempty)

tellInsertImage :: LocGraphic u -> EvalM u ()
tellInsertImage g = EvalM $ \s0 -> ((), s0, log1)
   where
     log1 = Log { insert_trace    = wrapH g
                , pen_trace       = emptyH  }

tellPenImage :: LocGraphic u -> EvalM u ()
tellPenImage g = EvalM $ \s0 -> ((), s0, log1)
   where
     log1 = Log { insert_trace    = emptyH
                , pen_trace       = wrapH g }


interpMove :: Num u => Vec2 u -> EvalM u () 
interpMove v = sets_ upd where
   upd = (\s i j k -> s { cumulative_disp = i ^+^ v
                        , cumulative_path = j `append` lineTo v 
                        , active_pen      = continuePen (lineTo v) k }) 
           <*> cumulative_disp <*> cumulative_path <*> active_pen

interpInsert :: Num u => LocGraphic u -> EvalM u ()
interpInsert gf = gets cumulative_disp >>= \v -> 
                  tellInsertImage (moveStart (displaceVec v) gf)

interpPenUp :: (Floating u, Ord u, LengthTolerance u, InterpretUnit u) 
            => EvalM u ()
interpPenUp = gets active_pen      >>= \pen ->
              gets cumulative_disp >>= \v -> 
              tellPen pen >> sets_ (upd v)
  where
    tellPen (_, PenStateUp)     = return ()
    tellPen (v, PenStateDown p) = tellPenImage $ penGraphic v p
    upd v                       = (\s -> s { active_pen = (v, PenStateUp) })



interpPenDown :: (Floating u, Ord u, LengthTolerance u, InterpretUnit u) 
            => EvalM u ()
interpPenDown = 
    gets active_pen      >>= \pen ->
    gets cumulative_disp >>= \v -> 
    tellPen pen >> sets_ (upd v)
  where
    tellPen (_, PenStateUp)     = return ()
    tellPen (v, PenStateDown p) = let gf = moveStart (displaceVec v) $ drawPath p 
                                  in tellPenImage (ignoreAns gf)
    upd v                       = (\s -> let pst = PenStateDown emptyRelPath
                                         in s { active_pen = (v, pst) } )




execPathSpec :: (Floating u, Ord u, LengthTolerance u, InterpretUnit u) 
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
                                        

fillPathSpec :: (Floating u, Ord u, LengthTolerance u, InterpretUnit u) 
             => PathSpec u -> LocImage AbsPath u
fillPathSpec spec = promoteR1 $ \start -> 
    let (rp,_,deco) = execPathSpec spec
        ap          = toAbsPath start rp
    in Abs.toPrimPath ap >>= \pp -> 
       replaceAns ap $ decorate (filledPath pp) (deco `at` start)


strokePathSpec :: (Floating u, Ord u, LengthTolerance u, InterpretUnit u) 
             => PathSpec u -> LocImage AbsPath u
strokePathSpec spec = promoteR1 $ \start -> 
    let (rp,img,deco) = execPathSpec spec
        ap            = toAbsPath start rp
    in replaceAns ap $ decorate (img `at` start) (deco `at` start)


drawPath :: (Floating u, Ord u, LengthTolerance u, InterpretUnit u) 
         => RelPath u -> LocImage AbsPath u 
drawPath rp = promoteR1 $ \start -> 
    let ap = toAbsPath start rp
    in replaceAns ap $ Abs.toPrimPath ap >>= openStroke




    
--------------------------------------------------------------------------------
-- user commands


move :: (u,u) -> Action u
move (x,y) = Move $ V2 x y

insert :: LocGraphic u -> Action u
insert = Insert

pen_up :: Action u 
pen_up = PenUp


pen_down :: Action u 
pen_down = PenDown