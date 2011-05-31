{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Tree.TreeBuilder
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Monadic building - trees with references for indirect 
-- node-to-node connections and decoration
--
--------------------------------------------------------------------------------

module Wumpus.Tree.TreeBuilder
  (
    AbsTreeSpec
  , TreeSpec
  , TbNode

  , ref 
  , leaf
  , root
  , mkleaf
  , linkref    

  , drawTreeSpec

  ) where

import Wumpus.Tree.Base
import Wumpus.Tree.Design

import Wumpus.Drawing.Basis.TraceGraphic        -- package: wumpus-drawing

import Wumpus.Basic.Kernel                      -- package: wumpus-basic
import Wumpus.Basic.Utils.HList

import Wumpus.Core                              -- package: wumpus-core

import Control.Applicative
import qualified Data.IntMap as IM
import Data.Monoid
import Data.Tree


data TbNode u a = RefNode Int (LocImage u a)
                | PlainNode (LocImage u a)

type instance DUnit (TbNode u a) = u



type RefTree u a = Tree (TbNode u a)

type CoordRefTree u a = Tree (Point2 u, TbNode u a)



type LinkDraw u node  = (node -> node -> Graphic u)


-- | This allows special connectors or edge labels.
--
type LinkRef u node = (Int,Int, LinkDraw u node)


newtype TreeSpec node u a = TreeSpec { 
          getTreeSpec :: Int -> (a, Int, H (LinkRef u node)) }

type instance MonUnit (TreeSpec node u a) = u


type AbsTreeSpec u node = TreeSpec node u (RefTree u node)


-- Functor

instance Functor (TreeSpec node u) where
  fmap f ma = TreeSpec $ \s0 -> let (a,s1,w1) = getTreeSpec ma s0 
                                in (f a, s1, w1)


-- Applicative

instance Applicative (TreeSpec node u) where
  pure a    = TreeSpec $ \s0  -> (a, s0, mempty)
  mf <*> ma = TreeSpec $ \s0 -> 
                let (f,s1,w1) = getTreeSpec mf s0
                    (a,s2,w2) = getTreeSpec ma s1
                in (f a, s2, w1 `mappend` w2)


-- Monad

instance Monad (TreeSpec node u) where
  return a = TreeSpec $ \s0 -> (a, s0, mempty)
  ma >>= k = TreeSpec $ \s0 -> let (a,s1,w1) = getTreeSpec ma s0
                                   (b,s2,w2) = (getTreeSpec . k) a s1
                               in (b,s2, w1 `mappend` w2)


runTreeSpec :: AbsTreeSpec u node -> (RefTree u node, [LinkRef u node])
runTreeSpec ma = let (a,_,w1) = getTreeSpec ma 0 
                 in (a, toListH w1)


ref :: LocImage u node -> TreeSpec node u (TbNode u node)
ref img = TreeSpec $ \s0 -> (RefNode s0 img, s0+1, mempty)

leaf :: TbNode u node -> RefTree u node 
leaf nod = Node nod []

root :: TbNode u node -> [RefTree u node ] -> RefTree u node 
root n1 xs = Node n1 xs

mkleaf :: LocImage u node -> RefTree u node
mkleaf img = Node (PlainNode img) []


linkref :: TbNode u node -> TbNode u node -> LinkDraw u node -> TreeSpec node u ()
linkref (RefNode ix _) (RefNode jx _) fn = TreeSpec $ \s0 -> 
   ((), s0, wrapH $ (ix,jx, fn))

linkref _              _              _  = TreeSpec $ \s0 -> ((), s0, mempty)



-------------------------------------------------------------
-- 




-- | Map for indexed objects that support taking anchors.
--
type ObjectMap node = IM.IntMap node



drawTreeSpec :: ( Real u, Floating u, InterpretUnit u
                , DrawingCtxM m, TraceM m, u ~ MonUnit (m ())
                )
             => TreeProps u node -> Point2 u 
             -> AbsTreeSpec u node
             -> m ()
drawTreeSpec props rootpt ma = 
    let (rtree,links) = runTreeSpec ma
    in  makeCoordRefTree props rootpt rtree >>= \ctree -> 
        askDC >>= \ctx ->
        let prim = rawBuildPrim ctx props ctree links
        in trace prim 

makeCoordRefTree :: ( Real u, Floating u, InterpretUnit u
                    , DrawingCtxM m, u ~ MonUnit (m ())
                    )
                 => TreeProps u a -> Point2 u -> RefTree u a 
                 -> m (CoordRefTree u a)
makeCoordRefTree props (P2 x y) tree = 
    scaleTree sx sy (design tree) >>= \ans -> return $ moveTree $ orient ans
  where
    orient   = orientateTree (tp_direction props)
    moveTree = fmap (\(a,b) -> (displace (V2 x y) a, b))
    sx       = tp_sibling_distance props
    sy       = tp_level_distance props


-- Whoa - have to be very careful about producing something that 
-- is consistent with the DrawingContext.
-- 
-- If we fork the DrawingContext we don\'t want to produce a 
-- Graphic as a Graphic should be able to be (re-)drawn various 
-- times in updated Contexts and reflect the changes each time.
--
-- Need a custom draw function... 



rawBuildPrim :: InterpretUnit u 
             => DrawingContext -> TreeProps u node 
             -> CoordRefTree u node
             -> [LinkRef u node]
             -> HPrim u
rawBuildPrim ctx props tree links = 
    let (_,w1,o) = runBuilder ctx props (node1 tree) 
        w2       = foldr (fn o) mempty links
    in w1 `mappend` w2
  where
    fn imap (i,j,drawF) acc = case (IM.lookup i imap, IM.lookup j imap) of
        (Just a, Just b) -> let (PrimW o _) = runImage ctx (drawF a b)
                            in singleH o `mappend` acc
        _                -> acc


node1 :: InterpretUnit u
      => CoordRefTree u node -> Builder node u node
node1 (Node (pt, RefNode ix gf) kids) = 
    let img = applyLoc gf pt
    in do { a <- tellImage img
          ; addNodeRef ix a
          ; as <- mapM node1 kids
          ; conn <- currentConnector
          ; tellImage_ (conn a as)
          ; return a
          }

node1 (Node (pt, PlainNode gf) kids) = 
    let img = applyLoc gf pt
    in do { a <- tellImage img
          ; as <- mapM node1 kids
          ; conn <- currentConnector
          ; tellImage_ (conn a as)
          ; return a
          }


--------------------------------------------------------------------------------
-- Here a dependency on MTL would be useful...

newtype MonBase node u a = MonBase { 
    getMonBase :: TreeProps u node -> ObjectMap node -> (a, ObjectMap node) }

newtype Builder node u a = Builder { 
    getBuilder :: TraceGraphicT u (MonBase node u) a } 

type instance MonUnit (MonBase node u a) = u
type instance MonUnit (Builder node u a) = u


-- Functor

instance Functor (MonBase node u) where
  fmap f ma = MonBase $ \env s0 -> let (a,s1) = getMonBase ma env s0 
                                   in (f a, s1)

instance Functor (Builder node u) where
  fmap f = Builder . fmap f . getBuilder 


-- Applicative

instance Applicative (MonBase node u) where
  pure a    = MonBase $ \_   s0  -> (a, s0)
  mf <*> ma = MonBase $ \env s0 -> 
                let (f,s1) = getMonBase mf env s0
                    (a,s2) = getMonBase ma env s1
                in (f a, s2)

instance Applicative (Builder node u) where
  pure a    = Builder $ pure a
  mf <*> ma = Builder $ getBuilder mf <*> getBuilder ma 


-- Monad

instance Monad (MonBase node u) where
  return a = MonBase $ \_   s0 -> (a, s0)
  ma >>= k = MonBase $ \env s0 -> let (a,s1) = getMonBase ma env s0
                                   in (getMonBase . k) a env s1


instance Monad (Builder node u) where
  return a = Builder $ return a
  ma >>= f = Builder $ getBuilder ma >>= getBuilder . f


-- TraceGraphicM

instance TraceGraphicM (Builder node u) where
  tellImage img = Builder $ tellImage img


-- DrawingCtxM

instance DrawingCtxM (Builder node u) where
  askDC           = Builder $ askDC
  asksDC f        = Builder $ asksDC f
  localize upd ma = Builder $ localize upd (getBuilder ma)



liftBSt :: MonBase node u a -> Builder node u a
liftBSt ma = Builder $ liftTraceGraphicT ma

addNodeRef :: Int -> node -> Builder node u ()
addNodeRef i a = liftBSt inside
  where
    inside = MonBase $ \_ s0 -> ((), IM.insert i a s0)


currentConnector :: InterpretUnit u 
                 => Builder node u (node -> [node] -> Graphic u)
currentConnector = liftBSt inside >>= getTreeConnector
  where
    inside = MonBase $ \env s0 -> (env,s0)


runMonBase :: TreeProps u node -> MonBase node u a -> (a, ObjectMap node)
runMonBase props ma = getMonBase ma props mempty


runBuilder :: DrawingContext 
           -> TreeProps u node 
           -> (Builder node u a) 
           -> (a, HPrim u, ObjectMap node)
runBuilder ctx props ma = 
    let ((a,wp),s) = runMonBase props (runTraceGraphicT ctx (getBuilder ma)) in (a,wp,s)

