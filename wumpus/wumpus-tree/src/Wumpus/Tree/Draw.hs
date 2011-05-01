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


  ) where

import Wumpus.Tree.Base
import Wumpus.Tree.Design


import Wumpus.Basic.Kernel                      -- package: wumpus-basic

import Wumpus.Core                              -- package: wumpus-core


import Data.Tree hiding ( drawTree )







drawTree :: (Real u, Floating u, InterpretUnit u) 
         => TreeProps u a -> Tree (LocImage u a) -> LocGraphic u
drawTree props tree =  
    runDesign props tree >>= \tree1 -> 
    locGraphic_ $ drawStep props tree1


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
dblelaborate :: LocImage u a -> LocImage u b -> (a -> b -> Graphic u) -> LocImage u a
dblelaborate ma mb fn = promoteR1 $ \pt -> 
    apply1R1 ma pt >>= \(Ans o1 x) -> 
    apply1R1 mb pt >>= \(Ans o2 xs) ->
    fn x xs  >>= \(Ans o3 _) ->
    return $ Ans (o1 `oplus` o2 `oplus` o3) x

    -- potentially we need a function like elaborate that also 
    -- transforms the answer...
    -- Also arg order of elaborate and decorate might be better 
    -- flipped.






-- | Transform a tree where each node is a LocImage into a tree
-- where each LocImage is displaced by the necessary coordinate
-- so it can be drawn.
--
runDesign :: InterpretUnit u 
          => TreeProps u a  -> Tree (LocImage u a) 
          -> Query (Tree (LocImage u a))
runDesign props tree =  
    makeScalingFun props >>= \sf -> 
    return $ fmap (fn . bimapL sf) $ orientateTree dir $ design tree
  where
    fn ((P2 x y), gf) = moveStart (displaceVec (V2 x y)) gf
    dir               = tp_direction props


makeScalingFun :: InterpretUnit u 
               => TreeProps u a -> Query (Point2 UW -> Point2 u)
makeScalingFun (TreeProps { tp_scale_in_x = sx, tp_scale_in_y = sy }) = 
   getFontSize >>= \sz -> 
   return (\(P2 x y) -> let ux = sx * (dinterp sz $ realToFrac x)
                            uy = sy * (dinterp sz $ realToFrac y)
                        in P2 ux uy)




