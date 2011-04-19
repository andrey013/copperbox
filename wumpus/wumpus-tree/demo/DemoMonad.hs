{-# OPTIONS -Wall #-}


module DemoMonad where

import Wumpus.Tree
import Wumpus.Tree.TreeBuildMonad


import Wumpus.Drawing.Dots.AnchorDots           -- package: wumpus-drawing
import Wumpus.Drawing.Colour.SVGColours
import Wumpus.Drawing.Text.StandardFontDefs

import Wumpus.Basic.Kernel                      -- package: wumpus-basic
import Wumpus.Basic.System.FontLoader

import Wumpus.Core                              -- package: wumpus-core


import System.Directory


main :: IO ()
main = simpleFontLoader main1 >> return ()

main1 :: FontLoader -> IO ()
main1 loader = do
    createDirectoryIfMissing True "./out/" 
    base_metrics <- loader [ Right helvetica_family ]
    printLoadErrors base_metrics
    let pic1 = runCtxPictureU (makeCtx 18 base_metrics) tree_drawing1
    writeEPS "./out/mon_tree01.eps"  pic1
    writeSVG "./out/mon_tree01.svg"  pic1

    let pic2 = runCtxPictureU (makeCtx 24 base_metrics) tree_drawing2
    writeEPS "./out/mon_tree02.eps"  pic2
    writeSVG "./out/mon_tree02.svg"  pic2


makeCtx :: FontSize -> FontLoadResult -> DrawingContext
makeCtx sz m = set_font times_roman $ metricsContext sz m


-- Note - @label@ in TreeMonad is a waste of a valuable name...

tree1 :: TreeBuild u (TreeSpec Char)
tree1 = return $ 
    branch (label 'A') [branch (label 'B') bs, branch (label 'F') gs]
  where
    bs = [leaf $ label 'C', leaf $ label 'D', leaf $ label 'E']
    gs = [branch (label 'G') [ leaf $ label 'H'
                             , leaf $ label 'I'
                             , leaf $ label 'J' ] ]


tree_drawing1 :: CtxPicture
tree_drawing1 = udrawTracing (0::Double) $ 
    drawScaledTree (uniformSF 30) zeroPt $ runTreeBuild charNode tree1

{-
-- Don\'t necessarily need @annotate@ from TreeMonad ...
--
nodeanno :: InterpretUnit u 
         => DotLocImage u -> Anchor u -> DotLocImage u
nodeanno img ancr = lift0R1 ancr >>= \pt -> annotate img (deco pt)
  where
    deco pt a = promoteR1 $ \_ -> plainTextLine "deco" `at` pt
-}

tree2 :: (Real u, Floating u, InterpretUnit u) 
      => TreeBuild u (ZTreeSpec u)
tree2 = do
    special   <- nodeId $ dotText "a"
    rightmost <- nodeId $ dotText "z"
    let bs = [zleaf, zleaf, zleaf]
    let gs = [zleaf, zleaf, leaf $ rightmost ]
    return $ 
        branch special [zbranch bs, zleaf, zbranch gs]




tree_drawing2 :: CtxPicture
tree_drawing2 = udrawTracing (0::Double) $ do
    draw $ plainTextLine "Tree 2" `at` zeroPt
    draw $ filledDisk 2      `at` displaceH (-40) tree_ogin 
    draw $ filledDisk 2      `at` tree_ogin
    draw $ filledDisk 2      `at` displaceH   40  tree_ogin
    drawScaledTreeD (uniformSF 60) tree_ogin TREE_RIGHT 
         $ runTreeBuild (diskNode red) tree2 
  where
    tree_ogin = P2 240 0

