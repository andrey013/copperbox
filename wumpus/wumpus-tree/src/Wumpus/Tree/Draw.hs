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


import Wumpus.Basic.Kernel                      -- package: wumpus-basic

import Wumpus.Core                              -- package: wumpus-core

import Data.AffineSpace                         -- package: vector-space

import Data.Tree hiding ( drawTree )




drawTree :: (Real u, Floating u, InterpretUnit u) 
         => TreeProps u a -> CoordTree (LocImage u a) -> LocGraphic u
drawTree props tree = locGraphic_ $ drawStep props tree



drawStep :: (Real u, Floating u, InterpretUnit u) 
         => TreeProps u a -> CoordTree (LocImage u a) -> LocImage u a
drawStep props (Node (P2 uwx uwy, gf) ns) = promoteR1 $ \proot ->
    tp_scale props uwx uwy >>= \v1 -> 
    getTreeConnector props >>= \conn ->
    let pt   = proot .+^ v1 
        imgs = sequenceLocImage $ map (drawStep props) ns
    in dblelaborate (apply1R1 gf pt) (apply1R1 imgs proot) conn
      

-- | This is not really a generally function (it returns only the 
-- first answer but consumes the second), so it doesn\'t belong
-- in Wumpus-Basic. However, it is a problematic that it needs to 
-- deconstruct the Ans directly - this suggests there is a need 
-- for a more general version of this combinator in Wumpus-Basic.
-- 
dblelaborate :: Image u a -> Image u b -> (a -> b -> Graphic u) -> Image u a
dblelaborate ma mb fn = 
    ma       >>= \(Ans o1 x) -> 
    mb       >>= \(Ans o2 xs) ->
    fn x xs  >>= \(Ans o3 _) ->
    return $ Ans (o1 `oplus` o2 `oplus` o3) x

orientateCoordTree :: TreeDirection -> CoordTree a -> CoordTree a
orientateCoordTree TREE_UP     = rotateAboutRoot pi
orientateCoordTree TREE_DOWN   = id
orientateCoordTree TREE_LEFT   = rotateAboutRoot (1.5*pi)
orientateCoordTree TREE_RIGHT  = rotateAboutRoot (0.5*pi)




