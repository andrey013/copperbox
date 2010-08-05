{-# OPTIONS -Wall #-}


module Demo01 where

import Wumpus.Tree

import Wumpus.Basic.SVGColours                  -- package: wumpus-basic

import Data.Tree hiding ( drawTree )
import System.Directory


tree1 :: Tree Char
tree1 = Node 'A' [Node 'B' bs, Node 'F' fs]
  where
   bs = [Node 'C' [], Node 'D' [], Node 'E' []]
   fs = [Node 'G' [Node 'H' [], Node 'I' [], Node 'J' []]]

tree2 :: Tree Char
tree2 = Node 'A' [Node 'B' bs, Node 'F' [], Node 'G' gs]
  where
   bs = [Node 'C' [], Node 'D' [], Node 'E' []]
   gs = [Node 'H' [], Node 'I' [], Node 'J' []]





main :: IO ()
main = createDirectoryIfMissing True "./out/"
    >> writeEPS_TreePicture "./out/tree01.eps"  pic1
    >> writeSVG_TreePicture "./out/tree01.svg"  pic1
    >> writeEPS_TreePicture "./out/tree02.eps"  pic2
    >> writeSVG_TreePicture "./out/tree02.svg"  pic2





pic1 :: TreePicture
pic1 = drawTreePicture charNode (uniformScaling 30) tree1

pic2 :: TreePicture
pic2 = drawTreePicture (circleNode red) (uniformScaling 30) tree2

