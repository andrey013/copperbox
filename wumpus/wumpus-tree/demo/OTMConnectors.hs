{-# OPTIONS -Wall #-}


module OTMConnectors where

import Wumpus.Tree

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
    writeEPS "./out/otm_conn01.eps"  pic1
    writeSVG "./out/otm_conn01.svg"  pic1


makeCtx :: FontSize -> FontLoadResult -> DrawingContext
makeCtx sz m = set_font times_roman $ metricsContext sz m



tree_pic1 :: CtxPicture
tree_pic1 = udrawTracing (0::Double) $ do
   
    drawl (P2 280 500) $ runTree props1 big_tree
    drawl (P2  30 50)  $ runTree props2 little_tree
    drawl (P2 120 50)  $ runTree (tree_direction TREE_UP    props2) little_tree
    drawl (P2 220 50)  $ runTree (tree_direction TREE_RIGHT props2) little_tree
    drawl (P2 420 50)  $ runTree (tree_direction TREE_LEFT  props2) little_tree
    drawl (P2 520 220) $ runTree props3 little_tree
    drawl (P2 520 50)  $ runTree props4 little_tree
  where
    props1 = standardTreeProps 30 40 familyOTMC
    props2 = standardTreeProps 30 40 familyOTMC
    props3 = standardTreeProps 30 40 blankOTMC
    props4 = standardTreeProps 30 40 splayOTMC


little_tree :: (Real u, Floating u, InterpretUnit u) 
            => Tree (DotLocImage u)
little_tree = fmap (const dotCircle) $ Node 'A' [Node 'B' bs, Node 'C' cs]
  where
    bs = [Node 'D' [], Node 'E' []]
    cs = [Node 'F' []]


-- This is the tree from Andrew Kennedy's 
-- /Functional Pearl Drawing Trees/
--
big_tree :: (Real u, Floating u, InterpretUnit u) 
         => Tree (DotLocImage u)
big_tree = fmap dotChar $ Node 'A' [a1, a2, a3]
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

