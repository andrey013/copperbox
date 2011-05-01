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
-- import Wumpus.Tree.TreeBuildMonad

import Wumpus.Drawing.Colour.SVGColours         -- package: wumpus-drawing
import Wumpus.Drawing.Dots.AnchorDots
import Wumpus.Drawing.Text.StandardFontDefs

import Wumpus.Basic.Kernel                      -- package: wumpus-basic
import Wumpus.Basic.System.FontLoader

import Wumpus.Core                              -- package: wumpus-core

import Data.Tree hiding ( drawTree )
import System.Directory


main :: IO ()
main = simpleFontLoader main1 >> return ()

main1 :: FontLoader -> IO ()
main1 loader = do
    createDirectoryIfMissing True "./out/" 
    base_metrics <- loader [ Right helvetica_family ]
    printLoadErrors base_metrics
    let pic1 = runCtxPictureU (makeCtx 18 base_metrics) tree_pic1
    writeEPS "./out/regular_tree01.eps"  pic1
    writeSVG "./out/regular_tree01.svg"  pic1


makeCtx :: FontSize -> FontLoadResult -> DrawingContext
makeCtx sz m = set_font times_roman $ metricsContext sz m



tree_pic1 :: CtxPicture
tree_pic1 = udrawTracing (0::Double) $ do
    --
    draw $ dcTextlabel "Tree 1:"        `at` (P2 0  550)
    drawl (P2 10 500) $ runTree props1 tree1
    --
    draw $ dcTextlabel "Tree 2:"       `at` (P2 200 550) 
    drawl (P2 300 550) $ runTree props2 tree2

    draw $ dcTextlabel "Tree 3:"       `at` (P2 0  410) 
    localize (set_font_size 12) $ 
        drawl (P2 280 410) $ runTree props3 tree3

    --
    draw $ dcTextlabel "Tree 4:"       `at` (P2 0  200)
    drawl (P2 80 200) $ runTree props4 tree4
    --
    draw $ dcTextlabel "Tree 5:"        `at` zeroPt
    drawl (P2 240 0) $ runTree props5 tree5
  where
    props1 = tree_direction TREE_RIGHT $ standardTreeProps 30 30 radialOTMC
    props2 = standardTreeProps 30 40 familyOTMC
    props3 = tree_direction TREE_DOWN $ standardTreeProps 25 25 familyOTMC
    props4 = standardTreeProps 20 30 radialOTMC
    props5 = standardTreeProps 20 30 radialOTMC


tree1 :: (Real u, Floating u, InterpretUnit u) 
      => Tree (DotLocImage u)
tree1 = fmap (const red_dot) $ Node 'A' [Node 'B' bs, Node 'F' fs]
  where
    bs = [Node 'C' [], Node 'D' [], Node 'E' []]
    fs = [Node 'G' [Node 'H' [], Node 'I' [], Node 'J' []]]

    red_dot = localize (fill_colour red) dotDisk

tree2 :: (Real u, Floating u, InterpretUnit u) 
      => Tree (DotLocImage u)
tree2 = fmap dotChar $ Node 'A' [Node 'B' bs, Node 'F' [], Node 'G' gs]
  where
    bs = [Node 'C' [], Node 'D' [], Node 'E' []]
    gs = [Node 'H' [], Node 'I' [], Node 'J' []]


-- This is the tree from Andrew Kennedy's 
-- /Functional Pearl Drawing Trees/
--
tree3 :: (Real u, Floating u, InterpretUnit u) 
      => Tree (DotLocImage u)
tree3 = fmap dotChar $ Node 'A' [a1, a2, a3]
  where
    a1 = Node 'B' [b1, b2]
    a2 = Node 'S' [b3,b4]
    a3 = Node 'o' [b5]
    b1 = Node 'C' [tleaf 'D', c1]
    b2 = Node 'P' [tleaf 'Q', tleaf 'R']
    b3 = Node 'T' [tleaf 'U', tleaf 'V', c2]
    b4 = Node 'e' [c3, tleaf 'h', c4]
    b5 = Node 'p' [c5, c6, tleaf '2']
    c1 = Node 'E' [d1]
    c2 = Node 'W' [d2, d3]
    c3 = Node 'f' [tleaf 'g']
    c4 = Node 'i' [d4]
    c5 = Node 'q' [tleaf 'r', tleaf 's', tleaf 't', tleaf 'u']
    c6 = Node 'v' [tleaf 'w', d5, tleaf '0', tleaf '1']
    d1 = Node 'F' [tleaf 'G', e1, tleaf 'M', e2]
    d2 = Node 'X' [tleaf 'Y']
    d3 = Node 'Z' [tleaf 'a', tleaf 'b', tleaf 'c', tleaf 'd']
    d4 = Node 'j' [tleaf 'k', tleaf 'l', tleaf 'm', tleaf 'n']
    d5 = Node 'x' [tleaf 'y', tleaf 'z'] 
    e1 = Node 'H' [tleaf 'I', tleaf 'J', tleaf 'K', tleaf 'L']
    e2 = Node 'N' [tleaf 'O']

tleaf :: a -> Tree a
tleaf a = Node a []


-- This is the tree (a) T3 from Buchheim, Junger and Leipert
-- /Improving Walker\'s Algorithm to Run in Linear Time/.
-- 
tree4 :: (Real u, Floating u, InterpretUnit u) 
      => Tree (DotLocImage u)
tree4 = fmap (const circ_dot) $ Node (1::Int) [a1, a2]
  where
    a1 = Node  2 [b1]
    a2 = Node  3 [b2, b3]
    b1 = Node  4 [c1]
    b2 = Node  5 [c2]
    b3 = Node  6 [tleaf 9, c3]
    c1 = Node  7 [d1]
    c2 = Node  8 [tleaf 12]
    c3 = Node 10 [d2] 
    d1 = Node 11 [tleaf 14]
    d2 = Node 13 [tleaf 15]

    circ_dot = localize (stroke_colour black) dotCircle

-- This is the tree (b) T3 from Buchheim, Junger and Leipert
-- /Improving Walker\'s Algorithm to Run in Linear Time/.
-- 
-- The generated picture is different - Wumpus-Tree only evenly
-- spaces the leaves, it looks like the trees in that paper are 
-- evenly spaced at the interior nodes too.
-- 
tree5 :: (Real u, Floating u, InterpretUnit u) 
      => Tree (DotLocImage u)
tree5 = fmap (const circ_dot) $
    Node (1::Int) [a1, tleaf 3, tleaf 4, tleaf 5, a2, tleaf 7, tleaf 8, tleaf 9, a3]
  where
    a1 = Node  2 [ tleaf 11, tleaf 12, tleaf 13, tleaf 14, tleaf 15, tleaf 16
                 , tleaf 17, tleaf 18, tleaf 19, tleaf 20, b1]
    a2 = Node  6 [ tleaf 22 ]
    a3 = Node 10 [ b2 ]
    b1 = Node 21 [ tleaf 24, tleaf 25, tleaf 26, tleaf 27, tleaf 28, tleaf 29
                 , tleaf 30, tleaf 31, tleaf 32, tleaf 33, tleaf 34]
    b2 = Node 23 [ tleaf 35 ]

    circ_dot = localize (stroke_colour black) dotCircle
