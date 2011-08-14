{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Tree.DrawLoc
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Drawing a Tree as a LocGraphic.
--
--------------------------------------------------------------------------------

module Wumpus.Tree.DrawLoc
  (
    
    AnnoNode
  , TreeSpec    
  , plainTree
  , treeDrawing

  , leaf
  , xleaf
  , tree
  , xtree

  ) where

import Wumpus.Tree.Base
import Wumpus.Tree.Design


import Wumpus.Basic.Kernel                      -- package: wumpus-basic

import Wumpus.Core                              -- package: wumpus-core

import Control.Applicative
import Data.Tree hiding ( drawTree )


data AnnoNode ix u a = PlainNode (LocImage u a)
                   | RefNode ix (LocImage u a)


type TreeSpec ix u a = Tree (AnnoNode ix u a)


plainTree :: (elt -> LocImage u a) -> Tree elt -> TreeSpec ix u a
plainTree gf = fmap (PlainNode . gf)


treeDrawing :: (Real u, Floating u, Translate node, InterpretUnit u, u ~ DUnit node)
            => TreeProps node u -> TreeSpec ix u node -> LocGraphic u
treeDrawing props t1 = promoteLoc $ \pt ->
    liftQuery (runDesign props t1) >>= \t2 -> applyLoc (phase1 props t2) pt



-- leaf should build a Data.Tree node with no kids...
--
leaf :: LocImage u a -> TreeSpec ix u a
leaf a = Node (PlainNode a) []

xleaf :: ix -> LocImage u a -> TreeSpec ix u a
xleaf ix a = Node (RefNode ix a) []


tree :: LocImage u a -> [TreeSpec ix u a] -> TreeSpec ix u a
tree a kids = Node (PlainNode a) kids

xtree :: ix -> LocImage u a -> [TreeSpec ix u a] -> TreeSpec ix u a
xtree ix a kids = Node (RefNode ix a) kids



phase1 :: (Translate node, InterpretUnit u, u ~ DUnit node)
       => TreeProps node u -> TreeSpec ix u node -> LocGraphic u
phase1 props t1 = ignoreAns $ runTreeMonad (step1 t1) props
  where
    step1 (Node nd []) = insert1 nd

    step1 (Node nd xs) = insert1 nd    >>= \r1 -> 
                         mapM step1 xs >>= \rs ->
                         drawConn r1 rs >>
                         return r1

    -- TODO ix
    insert1 (PlainNode gf)         = insertli zeroPt gf
    insert1 (RefNode _ gf)         = insertli zeroPt gf





-- | Transform a tree where each node is a LocImage into a tree
-- where each LocImage is displaced by the necessary coordinate
-- so it can be drawn.
--
runDesign :: (Real u, Floating u, InterpretUnit u)
          => TreeProps node u -> TreeSpec ix u a -> Query u (TreeSpec ix u a)
runDesign props t1 =  
     fmap post <$> designOrientateScale props t1
  where
    post ((P2 x y), PlainNode gf)   = PlainNode $ moveStart (vec x y) gf
    post ((P2 x y), RefNode ix gf)  = RefNode ix $ moveStart (vec x y) gf




designOrientateScale :: (Real u, Floating u, InterpretUnit u)
                     => TreeProps node u -> TreeSpec ix u a 
                     -> Query u (Tree (Point2 u, AnnoNode ix u a))
designOrientateScale props t1 =  
    scaleTree sx sy (design t1) >>= \ans -> return $ orientateTree dir ans
  where
    dir = tp_direction props
    sx  = tp_sibling_distance props
    sy  = tp_level_distance props


