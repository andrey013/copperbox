{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Tree.Draw
-- Copyright   :  (c) Stephen Tetley 2010-2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Drawing the tree using Wumpus-Basic.
--
--------------------------------------------------------------------------------

module Wumpus.Tree.Draw 
  (
    drawTree

  , orientateCoordTree

  ) where

import Wumpus.Tree.Base
import Wumpus.Tree.Design

import Wumpus.Drawing.Dots.AnchorDots           -- package: wumpus-drawing

import Wumpus.Basic.Kernel                      -- package: wumpus-basic

import Wumpus.Core                              -- package: wumpus-core

import Data.AffineSpace                         -- package: vector-space

import Control.Applicative
import Control.Monad
import qualified Data.IntMap as IntMap
import Data.Tree hiding ( drawTree )




drawTree :: (Real u, Floating u, InterpretUnit u) 
         => TreeProps u a -> CoordTree (LocImage u a) -> LocGraphic u
drawTree props tree = locGraphic_ $ drawStep props tree



drawStep :: (Real u, Floating u, InterpretUnit u) 
         => TreeProps u a -> CoordTree (LocImage u a) -> LocImage u a
drawStep props (Node (P2 uwx uwy, gf) ns) = promoteR1 $ \proot ->
    tp_scale props uwx uwy >>= \v1 -> 
    let pt   = proot .+^ v1 
        imgs = lsconcat $ map (drawStep props) ns
    in apply1R1 gf pt           >>= \(Ans o1 x) -> 
       apply1R1 imgs proot      >>= \(Ans o2 xs) ->
       tp_multiconn props x xs  >>= \(Ans o3 _) ->
       return $ Ans (o1 `oplus` o2 `oplus` o3) x


lsconcat :: InterpretUnit u => [LocImage u a] -> LocImage u [a]
lsconcat []      = pushR1 (replaceAns []) emptyLocGraphic
lsconcat (gf:gs) = promoteR1 $ \pt -> 
                   apply1R1 gf pt            >>= \(Ans o1 x) -> 
                   apply1R1 (lsconcat gs) pt >>= \(Ans o2 xs) ->
                   return $ Ans (o1 `oplus` o2) (x:xs)



orientateCoordTree :: TreeDirection -> CoordTree a -> CoordTree a
orientateCoordTree TREE_UP     = rotateAboutRoot pi
orientateCoordTree TREE_DOWN   = id
orientateCoordTree TREE_LEFT   = rotateAboutRoot (1.5*pi)
orientateCoordTree TREE_RIGHT  = rotateAboutRoot (0.5*pi)




