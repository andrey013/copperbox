{-# OPTIONS -Wall #-}


module Demo01 where

import Wumpus.Tree.Algorithm
import Wumpus.Tree.Draw

import Wumpus.Core                      -- package: wumpus-core
import Wumpus.Basic.Graphic             -- package: wumpus-basic

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



{- 
treeABC = Node 'o' [Node 'A' [], Node 'B' [], Node 'C' []]
treeAB  = Node 'o' [Node 'A' [], Node 'B' []]
-}

fx :: Double -> Double
fx = (*30.0)

fy :: Int -> Double
fy x = 30.0 * fromIntegral x



main :: IO ()
main = createDirectoryIfMissing True "./out/"
    >> writeEPS_latin1 "./out/tree01.eps"  pic1
    >> writeSVG_latin1 "./out/tree01.svg"  pic1
    >> writeEPS_latin1 "./out/tree02.eps"  pic2
    >> writeSVG_latin1 "./out/tree02.svg"  pic2





pic1 :: Picture Double
pic1 = drawGraphicU $ drawTree $ design (fx,fy) tree1

pic2 :: Picture Double
pic2 = drawGraphicU $ drawTree $ design (fx,fy) tree2

