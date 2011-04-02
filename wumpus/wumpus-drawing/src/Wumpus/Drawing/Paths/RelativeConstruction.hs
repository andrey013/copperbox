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
  ) where

import Wumpus.Drawing.Paths.Relative
import Wumpus.Drawing.Paths.Absolute ( AbsPath )
import qualified Wumpus.Drawing.Paths.Absolute as Abs


import Wumpus.Basic.Kernel                      -- package: wumpus-basic
import Wumpus.Basic.Utils.HList

import Wumpus.Core                              -- package: wumpus-core


import Data.VectorSpace                         -- package: vector-space

import Control.Applicative
-- import Data.List



data Action u = Move (Vec2 u)
              | Insert (LocGraphic u)

data St u = St 
      { cumulative_disp   :: Vec2 u
      , cumulative_path   :: RelPath u
      }

-- | Evaluation is two States - a vector for cummulative 
-- displacement and a cummulative path - plus one Writer - a trace 
-- of TikZ-like @insert@ commands.
--
data EvalM u a = EvalM { getEvalM :: St u -> (a, St u, H (LocGraphic u)) }


instance Functor (EvalM u) where
  fmap f mf = EvalM $ \s0 -> let (a, s1, w) = getEvalM mf s0
                             in (f a, s1, w)


instance Applicative (EvalM u) where
  pure a    = EvalM $ \st0   -> (a, st0, emptyH)
  mf <*> ma = EvalM $ \st0 -> 
                let (f,st1,w1) = getEvalM mf st0
                    (a,st2,w2) = getEvalM ma st1
                in (f a, st2, w1 `appendH` w2)

instance Monad (EvalM u) where
  return a  = EvalM $ \st0   -> (a, st0, emptyH)
  ma >>= k  = EvalM $ \st0 -> 
                let (a,st1,w1) = getEvalM ma st0
                    (b,st2,w2) = (getEvalM . k) a st1
                in (b, st2, w1 `appendH` w2)


execEvalM :: InterpretUnit u => EvalM u a -> (RelPath u, LocGraphic u) 
execEvalM mf = post $ getEvalM mf zeroSt
  where
    post (_,st,hlist) = let gf = altconcat emptyLocGraphic $ toListH hlist
                        in (cumulative_path st , gf)


zeroSt :: Num u => St u
zeroSt = St { cumulative_disp   = V2 0 0
            , cumulative_path   = emptyRelPath
            }


perform :: InterpretUnit u => [Action u] -> (RelPath u, LocGraphic u)
perform []     = (emptyRelPath, emptyLocGraphic)
perform (x:xs) = execEvalM (go x xs)
  where
    go a []     = step1 a 
    go a (b:bs) = step1 a >> go b bs 
    
    step1 (Move v)    = interpMove v
    step1 (Insert gf) = interpInsert gf

-- sets :: (St u -> (a, St u)) -> EvalM u a
-- sets f = EvalM $ \s0  -> let (a,s1) = f s0 in (a, s1, emptyH)

sets_ :: (St u -> St u) -> EvalM u ()
sets_ f = EvalM $ \s0  -> ((), f s0, emptyH)

-- get :: EvalM u (St u)
-- get = EvalM $ \s0 -> (s0,s0,emptyH)

gets :: (St u -> a) -> EvalM u a
gets f = EvalM $ \s0 -> (f s0, s0, emptyH)

tell :: LocGraphic u -> EvalM u ()
tell g = EvalM $ \s0 -> ((), s0, wrapH g)

interpMove :: Num u => Vec2 u -> EvalM u () 
interpMove v = sets_ upd where
   upd = (\s i j -> s { cumulative_disp = i ^+^ v
                      , cumulative_path = j `append` lineTo v }) 
           <*> cumulative_disp <*> cumulative_path

interpInsert :: Num u => LocGraphic u -> EvalM u ()
interpInsert gf = gets cumulative_disp >>= \v -> 
                  tell (moveStart (displaceVec v) gf)


evalPath :: (Floating u, Ord u, LengthTolerance u, InterpretUnit u) 
         => [Action u] -> LocImage AbsPath u
evalPath xs = decorate img deco
  where
    (rp,deco) = perform xs
    img       = drawPath rp
    


drawPath :: (Floating u, Ord u, LengthTolerance u, InterpretUnit u) 
         => RelPath u -> LocImage AbsPath u 
drawPath rp = promoteR1 $ \start -> 
    let ap = toAbsPath start rp
    in replaceAns ap $ Abs.toPrimPath ap >>= openStroke
    


move :: (u,u) -> Action u
move (x,y) = Move $ V2 x y

insert :: LocGraphic u -> Action u
insert = Insert