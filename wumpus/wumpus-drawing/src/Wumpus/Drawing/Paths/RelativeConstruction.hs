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

    Action
  , evalPath
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
  mempty = NoLog
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


-- Note - active pen needs flushing.
--
execEvalM :: (Floating u, Ord u, LengthTolerance u, InterpretUnit u) 
          => EvalM u a -> (RelPath u, LocGraphic u, LocGraphic u) 
execEvalM mf = post $ getEvalM mf zeroSt
  where
    mkGraphic     = altconcat emptyLocGraphic . toListH
    post (_,st,w) = let path_img = mkGraphic $ pen_trace w
                        ins_img  = mkGraphic $ insert_trace w
                        path_ans = cumulative_path st 
                    in case active_pen st of
                         (_, PenStateUp)     -> (path_ans, path_img, ins_img)
                         (v, PenStateDown p) -> let g1 = penGraphic v p 
                                                in ( path_ans 
                                                   , path_img `oplus` g1
                                                   , ins_img )
                                             

zeroSt :: Num u => St u
zeroSt = St { cumulative_disp   = V2 0 0
            , cumulative_path   = emptyRelPath
            , active_pen        = (V2 0 0, PenStateDown emptyRelPath)
            }


perform :: (Floating u, Ord u, LengthTolerance u, InterpretUnit u) 
        => [Action u] -> (RelPath u, LocGraphic u, LocGraphic u)
perform []     = (emptyRelPath, emptyLocGraphic, emptyLocGraphic)
perform (x:xs) = execEvalM (go x xs)
  where
    go a []     = step1 a 
    go a (b:bs) = step1 a >> go b bs 
    
    step1 (Move v)    = interpMove v
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




evalPath :: (Floating u, Ord u, LengthTolerance u, InterpretUnit u) 
         => [Action u] -> LocImage AbsPath u
evalPath xs = promoteR1 $ \start -> 
    replaceAns (toAbsPath start rp) $ decorate img deco `at` start
  where
    (rp,img,deco) = perform xs
        


drawPath :: (Floating u, Ord u, LengthTolerance u, InterpretUnit u) 
         => RelPath u -> LocImage AbsPath u 
drawPath rp = promoteR1 $ \start -> 
    let ap = toAbsPath start rp
    in replaceAns ap $ Abs.toPrimPath ap >>= openStroke
    


move :: (u,u) -> Action u
move (x,y) = Move $ V2 x y

insert :: LocGraphic u -> Action u
insert = Insert

pen_up :: Action u 
pen_up = PenUp


pen_down :: Action u 
pen_down = PenDown