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
    
    runTreeLoc

  ) where

import Wumpus.Tree.Base
import Wumpus.Tree.Design


import Wumpus.Basic.Kernel                      -- package: wumpus-basic

import Wumpus.Core                              -- package: wumpus-core


import Data.Tree hiding ( drawTree )


--
-- DESIGN NOTE 
--
-- Only simplistic trees can be drawn as LocGraphics.
--
-- Technically, this is because LocImages only support 
-- /production/ of /answers/ and not their /introspection/, so
-- we cannot query anchors directly[*] during construction. Thus 
-- we can\'t have /graph-links/ which need /located/ anchors.
--
-- [*] Though we can use @dblelaborate@ for a special case.
--


-- | Build a LocGraphic from a @Data.Tree@.
--
-- Nodes support custom drawing as the value of the /label/ at 
-- each node is interpreted (naturally, all node drawings must 
-- be of the same type). 
--
runTreeLoc :: (Real u, Floating u, InterpretUnit u) 
           => TreeProps u a -> (elt -> LocImage u a) -> Tree elt 
           -> LocGraphic u
runTreeLoc props drawF tree = 
    let tree1 = fmap drawF tree
    in runDesign props tree1 >>= \ans -> 
       locGraphic_ (drawStep props ans)



drawStep :: (Real u, Floating u, InterpretUnit u) 
         => TreeProps u a -> Tree (LocImage u a) -> LocImage u a
drawStep props (Node gf ns) =
    getTreeConnector props >>= \conn ->
    let imgs = sequenceLocImage $ map (drawStep props) ns
    in dblelaborate gf imgs conn
      

-- | This is not really a generally function - the types are not
-- complementary and it returns only the first answer but consumes 
-- the second, so it doesn\'t belong in Wumpus-Basic. 
-- 
-- However, it is a problematic that it needs to 
-- deconstruct the Ans directly - this suggests there is a need 
-- for a more general version of this combinator in Wumpus-Basic.
-- 
dblelaborate :: LocImage u a -> LocImage u b 
             -> (a -> b -> Graphic u) 
             -> LocImage u a
dblelaborate ma mb fn = promoteR1 $ \pt -> 
    apply1R1 ma pt >>= \(Ans o1 x) -> 
    apply1R1 mb pt >>= \(Ans o2 xs) ->
    fn x xs  >>= \(Ans o3 _) ->
    return $ Ans (o1 `oplus` o2 `oplus` o3) x

    -- potentially we need a function like elaborate that also 
    -- transforms the answer...
    -- Also arg order of elaborate and decorate might be better 
    -- flipped.


designOrientateScale :: (Real u, Floating u, InterpretUnit u)
                     => TreeProps u a  -> Tree (LocImage u a) 
                     -> Query (Tree (Point2 u, LocImage u a))
designOrientateScale props tree =  
    scaleTree sx sy (design tree) >>= \ans -> return $ orientateTree dir ans
  where
    dir = tp_direction props
    sx  = tp_sibling_distance props
    sy  = tp_level_distance props



-- | Transform a tree where each node is a LocImage into a tree
-- where each LocImage is displaced by the necessary coordinate
-- so it can be drawn.
--
runDesign :: (Real u, Floating u, InterpretUnit u)
          => TreeProps u a  -> Tree (LocImage u a) 
          -> Query (Tree (LocImage u a))
runDesign props tree =  
    designOrientateScale props tree >>= \tree2 -> 
    return $ fmap fn tree2
  where
    fn ((P2 x y), gf) = moveStart (displaceVec (V2 x y)) gf


