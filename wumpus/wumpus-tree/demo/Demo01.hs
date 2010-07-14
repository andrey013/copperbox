{-# OPTIONS -Wall #-}


module Demo01 where

import Wumpus.Tree.Algorithm
import Wumpus.Tree.Draw

import Wumpus.Core                      -- package: wumpus-core
import Wumpus.Basic.Graphic             -- package: wumpus-basic

import Data.Maybe
import Data.Tree
import System.Directory


tree1 :: Tree Char
tree1 = Node 'A' [Node 'B' bs, Node 'F' fs]
  where
   bs = [Node 'C' [], Node 'D' [], Node 'E' []]
   fs = [Node 'G' [Node 'H' [], Node 'I' [], Node 'J' []]]



tree2 :: Tree (PNode Char)
tree2 = design tree1

tree3 :: Tree (LocNode Double Char)
tree3 = scaleTree ((*30.0), \x-> 30.0 * fromIntegral x) tree2

-- Note - not corret at the moment...
--
main :: IO ()
main = createDirectoryIfMissing True "./out/"
    >> writeEPS_latin1 "./out/tree01.eps" pic1
    >> writeSVG_latin1 "./out/tree01.svg" pic1


pic1 :: Picture Double
pic1 = fromMaybe errK $ drawGraphic $ draw tree3


errK :: a
errK = error "no picture"

