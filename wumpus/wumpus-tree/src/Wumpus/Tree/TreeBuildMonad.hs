{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Tree.TreeBuildMonad
-- Copyright   :  (c) Stephen Tetley 2010-2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Build a tree within a monad - the monad allows anchor references.
--
--------------------------------------------------------------------------------

module Wumpus.Tree.TreeBuildMonad
  (
    NodeId
  , ZNodeId
  , NodeAnno
  , NodeAnnoRefs
  
  , TreeBuild
  , TreeSpec
  , ZTreeSpec

  , TreeNodeAns
  , TreeBuildAns
  , runTreeBuild

  , regularBuild

  , nodeId
  , label

  , branch
  , zbranch
  , leaf
  , zleaf

  ) where

import Wumpus.Tree.Base

import Wumpus.Basic.Kernel                      -- package: wumpus-basic
import Wumpus.Drawing.Dots.AnchorDots


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
type ZNodeId u = NodeId (UNil u)

type NodeDrawRefs u = IntMap.IntMap (TreeNode u)

type NodeAnno u     = DotAnchor u -> Graphic u

type NodeAnnoRefs u = IntMap.IntMap (NodeAnno u)

data St u = St
      { uid_counter     :: Int
      , node_refs       :: NodeDrawRefs u
      , anno_refs       :: NodeAnnoRefs u
      }


zeroSt :: St u
zeroSt = St { uid_counter = 0, node_refs = mempty, anno_refs = mempty }


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


type TreeSpec a   = Tree (NodeId a)
type ZTreeSpec u  = TreeSpec (UNil u)


type TreeNodeAns  u = (TreeNode u, Maybe Int)
type TreeBuildAns u = (Tree (TreeNodeAns u), NodeAnnoRefs u)

-- | This is the @run@ function for the TreeBuild monad.
--
-- Note the monadic /command/ is type specialized to 
-- @(TreeSpec a)@, this is because evaluation in the TreeBuild
-- monad is only significant for producing a @Tree (TreeNode u)@.
--

runTreeBuild :: (Real u, Floating u, InterpretUnit u)
             => (a -> TreeNode u) -> TreeBuild u (TreeSpec a) -> TreeBuildAns u
runTreeBuild regDrawF ma = 
    let (a,s) = getTreeBuild ma zeroSt
        t1    = postRun regDrawF (a, node_refs s)
    in (t1, anno_refs s)



-- As the constructor to build NodeIds is not exposed a 
-- TreeBuild should not be able to refer to uninstantiated
-- nodes, however while the failure continuation should be 
-- unreachable we still need it in the code to make the 
-- IntMap.lookup total.
--
postRun :: (Real u, Floating u, InterpretUnit u)
        => (a -> TreeNode u) -> (TreeSpec a,NodeDrawRefs u) 
        -> Tree (TreeNode u, Maybe Int)
postRun regDrawF (tree1,table) = fmap changeNode tree1
  where
    changeNode (RegularNode a)  = (regDrawF a, Nothing)
    changeNode (NodeId ix)      = maybe fk (sk ix) $ IntMap.lookup ix table 
    
    sk ix                       = \a -> (a, Just ix)
    fk                          = (dotText "Error missing node", Nothing)
 


-- | Turn an ordinary @Data.Tree@ into a /regular/ 'TreeSpec'.
--
-- All nodes become /regular/ nodes, no nodes are /bound/. Thus
-- nodes cannot be annotated etc.
--  
regularBuild :: Tree a -> TreeBuild u (TreeSpec a)
regularBuild (Node a kids) =  
    Node (RegularNode a) <$> mapM regularBuild kids


nodeId :: TreeNode u -> TreeBuild u (NodeId a)
nodeId drawF = 
    TreeBuild $ \(St ix nodes annos) -> 
      let nodes' = IntMap.insert ix drawF nodes
      in (NodeId ix, St (ix+1) nodes' annos)

-- | Note - this is not /in/ the TreeBuild monad.
--
label :: a -> NodeId a 
label a = RegularNode a


{-
-- | Annotate a /node/ with a 'NodeAnno'.
-- 
-- Note - /regular/ nodes cannot be annotated, a node must be 
-- bound to a variable first with 'nodeId'.
--
-- Also this function is not so useful now Wumpus-Basic has
-- the @decorate@, @sdecorate@, and @adecorate@ functions.
-- 
annotate :: u ~ DUnit a => NodeId a -> NodeAnno u -> TreeBuild u ()
annotate (RegularNode _) _     = return ()
annotate (NodeId nid)    annoF = 
    TreeBuild $ \(St ix nodes annos) -> 
      let annos' = IntMap.insert nid annoF annos
      in ((), St ix nodes annos')
-}


branch :: NodeId a -> [TreeSpec a] -> TreeSpec a
branch uid kids = Node uid kids


-- | Default /branch/ - has children.
--
zbranch :: [ZTreeSpec u] -> ZTreeSpec u
zbranch kids = Node (RegularNode UNil) kids 

leaf :: NodeId a -> TreeSpec a
leaf uid = Node uid []

-- | Default /leaf/ - tree node with no children.
--
zleaf :: ZTreeSpec u
zleaf = Node (RegularNode UNil) []


            


