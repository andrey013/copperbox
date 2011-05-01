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

import Data.AffineSpace                         -- package: vector-space

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
    -- Also arg order of eleborate and decorate might be better 
    -- flipped.



