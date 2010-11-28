{-# OPTIONS -Wall #-}

-- Note - @main@ is more convoluted than would normally be 
-- expected as it supports both sources of glyph metrics - the 
-- GhostScript distribution or the Core 14 metrics from Adobe.
-- 
-- \"Real\" applications would be expected to choose one source. 
--
-- I-am-not-a-lawyer, but it does look as though the Adobe font
-- metrics are redistributable, the GhostScript metrics are 
-- seemingly redistributable under the same terms as the larger
-- GhostScript distribution.
-- 

module Demo01 where

import Wumpus.Tree

import Wumpus.Basic.Colour.SVGColours           -- package: wumpus-basic
import Wumpus.Basic.FontLoader.AfmLoader
import Wumpus.Basic.FontLoader.GSLoader
import Wumpus.Basic.Graphic
import Wumpus.Basic.SafeFonts

import Wumpus.Core                              -- package: wumpus-core

import FontLoaderUtils


import Data.Tree hiding ( drawTree )
import System.Directory


main :: IO ()
main = do 
    (mb_gs, mb_afm) <- processCmdLine default_font_loader_help
    createDirectoryIfMissing True "./out/"
    maybe gs_failk  makeGSPictures  $ mb_gs
--    maybe afm_failk makeAfmPictures $ mb_afm
  where
    gs_failk  = putStrLn "No GhostScript font path supplied..."
    afm_failk = putStrLn "No AFM v4.1 font path supplied..."

makeGSPictures :: FilePath -> IO ()
makeGSPictures font_dir = do 
    putStrLn "Using GhostScript metrics..."
    base_metrics <- loadGSMetrics font_dir ["Times-Roman"]
    --
    let pic1 = runDrawingU (makeCtx 18 base_metrics) tree_drawing1
    writeEPS "./out/tree01.eps"  pic1
    writeSVG "./out/tree01.svg"  pic1
    --
    let pic2 = runDrawingU (makeCtx 24 base_metrics) tree_drawing2
    writeEPS "./out/tree02.eps"  pic2
    writeSVG "./out/tree02.svg"  pic2
    --
    let pic3 = runDrawingU (makeCtx 14 base_metrics) tree_drawing3
    writeEPS "./out/tree03.eps"  pic3
    writeSVG "./out/tree03.svg"  pic3
    --
    let pic4 = runDrawingU (makeCtx 24 base_metrics) tree_drawing4
    writeEPS "./out/tree04.eps"  pic4
    writeSVG "./out/tree04.svg"  pic4
    --
    let pic5 = runDrawingU (makeCtx 24 base_metrics) tree_drawing5
    writeEPS "./out/tree05.eps"  pic5
    writeSVG "./out/tree05.svg"  pic5

makeCtx :: FontSize -> BaseGlyphMetrics -> DrawingContext
makeCtx sz m = fontface times_roman $ metricsContext sz m



tree_drawing1 :: DTreeDrawing
tree_drawing1 = drawScaledTree charNode  (uniformScaling 30) tree1

tree_drawing2 :: DTreeDrawing
tree_drawing2 = drawScaledTree (diskNode red) (uniformScaling 30) tree2

-- This should be drawn in the /family tree/ style...
-- 
tree_drawing3 :: DTreeDrawing
tree_drawing3 = drawScaledFamilyTree charNode (uniformScaling 30) tree3


tree_drawing4 :: DTreeDrawing
tree_drawing4 = drawScaledTree (circleNode black) (scaleFactors 20 30) tree4

tree_drawing5 :: DTreeDrawing
tree_drawing5 = drawScaledTree (circleNode black) (scaleFactors 20 30) tree5


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

-- This is the tree from Andrew Kennedy's 
-- /Functional Pearl Drawing Trees/
--
-- Currently Wumpus-tree cannot render in the /family tree/ style.
--
tree3 :: Tree Char
tree3 = Node 'A' [a1, a2, a3]
  where
    a1 = Node 'B' [b1, b2]
    a2 = Node 'S' [b3,b4]
    a3 = Node 'o' [b5]
    b1 = Node 'C' [leaf 'D', c1]
    b2 = Node 'P' [leaf 'Q', leaf 'R']
    b3 = Node 'T' [leaf 'U', leaf 'V', c2]
    b4 = Node 'e' [c3, leaf 'h', c4]
    b5 = Node 'p' [c5, c6, leaf '2']
    c1 = Node 'E' [d1]
    c2 = Node 'W' [d2, d3]
    c3 = Node 'f' [leaf 'g']
    c4 = Node 'i' [d4]
    c5 = Node 'q' [leaf 'r', leaf 's', leaf 't', leaf 'u']
    c6 = Node 'v' [leaf 'w', d5, leaf '0', leaf '1']
    d1 = Node 'F' [leaf 'G', e1, leaf 'M', e2]
    d2 = Node 'X' [leaf 'Y']
    d3 = Node 'Z' [leaf 'a', leaf 'b', leaf 'c', leaf 'd']
    d4 = Node 'j' [leaf 'k', leaf 'l', leaf 'm', leaf 'n']
    d5 = Node 'x' [leaf 'y', leaf 'z'] 
    e1 = Node 'H' [leaf 'I', leaf 'J', leaf 'K', leaf 'L']
    e2 = Node 'N' [leaf 'O']

leaf :: a -> Tree a
leaf a = Node a []


-- This is the tree (a) T3 from Buchheim, Junger and Leipert
-- /Improving Walker\'s Algorithm to Run in Linear Time/.
-- 
tree4 :: Tree Int
tree4 = Node 1 [a1, a2]
  where
    a1 = Node  2 [b1]
    a2 = Node  3 [b2, b3]
    b1 = Node  4 [c1]
    b2 = Node  5 [c2]
    b3 = Node  6 [leaf 9, c3]
    c1 = Node  7 [d1]
    c2 = Node  8 [leaf 12]
    c3 = Node 10 [d2] 
    d1 = Node 11 [leaf 14]
    d2 = Node 13 [leaf 15]

-- This is the tree (b) T3 from Buchheim, Junger and Leipert
-- /Improving Walker\'s Algorithm to Run in Linear Time/.
-- 
-- The generated picture is different - Wumpus-Tree only evenly
-- spaces the leaves, it looks like the trees in that paper are 
-- evenly spaced at the interior nodes too.
-- 
tree5 :: Tree Int
tree5 = Node 1 [a1, leaf 3, leaf 4, leaf 5, a2, leaf 7, leaf 8, leaf 9, a3]
  where
    a1 = Node  2 [leaf 11, leaf 12, leaf 13, leaf 14, leaf 15, leaf 16
                 , leaf 17, leaf 18, leaf 19, leaf 20, b1]
    a2 = Node  6 [leaf 22]
    a3 = Node 10 [b2]
    b1 = Node 21 [leaf 24, leaf 25, leaf 26, leaf 27, leaf 28, leaf 29
                 ,leaf 30, leaf 31, leaf 32, leaf 33, leaf 34]
    b2 = Node 23 [leaf 35]