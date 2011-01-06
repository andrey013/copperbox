{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Tree.TreeBuildMonad
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Common types, ...
--
--------------------------------------------------------------------------------

module Wumpus.Tree.TreeBuildMonad
  (
    NodeId
  , ZNodeId
  
  , TreeBuild
  , TreeSpec
  , ZTreeSpec

  , runTreeBuild

  , nodeId
  , label
  , branch
  , zbranch
  , leaf
  , zleaf

  ) where

import Wumpus.Tree.Base

-- import Wumpus.Basic.Kernel                      -- package: wumpus-basic
import Wumpus.Drawing.Dots.AnchorDots

import Wumpus.Core                              -- package: wumpus-core

import Control.Applicative
import qualified Data.IntMap as IntMap
import Data.Monoid
import Data.Tree





-- | Nodes can be bound with @(>>=)@ or in the do-notation before 
-- they are drawn. This is similar to the concept of /embedded/ 
-- nodes in TikZ. Bound nodes can be referenced by their anchors 
-- e.g. to give them an extra annotation.
--
-- This opaque type represents bound nodes and regular nodes that 
-- are just drawn and cannot be annotated.
-- 
data NodeId a = NodeId Int
              | RegularNode a
  deriving (Eq)

-- The default node type. With this type, regular nodes have no
-- payload so can only be drawn as some common graphic e.g. a 
-- filled or stroked disk.
--
type ZNodeId = NodeId ()

type NodeDrawRefs u = IntMap.IntMap (TreeNode u)

data St u = St
      { uid_counter     :: Int
      , node_refs       :: NodeDrawRefs u
      }

zeroSt :: St u
zeroSt = St { uid_counter = 0, node_refs = mempty }




newtype TreeBuild u a = TreeBuild { getTreeBuild :: St u -> (a, St u) }

instance Functor (TreeBuild u) where
  fmap f ma = TreeBuild $ \s -> let (a,s1) = getTreeBuild ma s in (f a, s1)

instance Applicative (TreeBuild u) where
  pure a    = TreeBuild $ \s -> (a,s)
  mf <*> ma = TreeBuild $ \s -> let (f,s1) = getTreeBuild mf s
                                    (a,s2) = getTreeBuild ma s1
                                in (f a,s2)

instance Monad (TreeBuild u) where
  return a  = TreeBuild $ \s -> (a,s)
  ma >>= k  = TreeBuild $ \s -> let (a,s1) = getTreeBuild ma s 
                                in getTreeBuild (k a) s1 




type TreeSpec a = Tree (NodeId a)
type ZTreeSpec  = TreeSpec ()

-- | This is the @run@ function for the TreeBuild monad.
--
-- Note the monadic /command/ is type specialized to 
-- @(TreeSpec a)@, this is because evaluation in the TreeBuild
-- monad is only significant for producing a @Tree (TreeNode u)@.
--

runTreeBuild :: (Real u, Floating u, FromPtSize u)
               => (a -> TreeNode u) -> TreeBuild u (TreeSpec a) -> Tree (TreeNode u)
runTreeBuild regDrawF ma = 
    let (a,s) = getTreeBuild ma zeroSt in postRun regDrawF (a, node_refs s)


-- As the constructor to build NodeIds is not exposed a 
-- TreeBuild should not be able to refer to uninstantiated
-- nodes, however while the failure continuation should be 
-- unreachable we still need it in the code to make the 
-- IntMap.lookup total.
--
postRun :: (Real u, Floating u, FromPtSize u)
        => (a -> TreeNode u) -> (TreeSpec a,NodeDrawRefs u) -> Tree (TreeNode u)
postRun regDrawF (tree1,table) = fmap changeNode tree1
  where
    changeNode (RegularNode a)  = regDrawF a
    changeNode (NodeId ix)      = maybe fk id $ IntMap.lookup ix table 
    
    fk                          = dotText "Error missing node"
 


nodeId :: TreeNode u -> TreeBuild u (NodeId a)
nodeId drawF = TreeBuild $ \(St uid nodes) -> 
                 let nodes' = IntMap.insert uid drawF nodes
                 in (NodeId uid, St (uid+1) nodes')

label :: a -> NodeId a 
label a = RegularNode a



branch :: NodeId a -> [TreeSpec a] -> TreeSpec a
branch uid kids = Node uid kids


-- | Default /branch/ - has children.
--
zbranch :: [ZTreeSpec] -> ZTreeSpec
zbranch kids = Node (RegularNode ()) kids 

leaf :: NodeId a -> TreeSpec a
leaf uid = Node uid []

-- | Default /leaf/ - tree node with no children.
--
zleaf :: ZTreeSpec
zleaf = Node (RegularNode ()) []

            


