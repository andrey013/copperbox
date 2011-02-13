{-# OPTIONS -Wall #-}


module DemoMonad where

import FontLoaderUtils

import Wumpus.Tree
import Wumpus.Tree.TreeBuildMonad

import Wumpus.Basic.Kernel                      -- package: wumpus-basic
import Wumpus.Basic.System.FontLoader.Afm
import Wumpus.Basic.System.FontLoader.GhostScript
import Wumpus.Drawing.Dots.AnchorDots
import Wumpus.Drawing.Colour.SVGColours
import Wumpus.Drawing.Text.SafeFonts

import Wumpus.Core                              -- package: wumpus-core


import System.Directory


main :: IO ()
main = do
    (mb_gs, mb_afm) <- processCmdLine default_font_loader_help
    createDirectoryIfMissing True "./out/"
    case (mb_gs, mb_afm) of       
      (Just dir, _) -> do { putStrLn "Using GhostScript metrics..."
                          ; metrics <- loadGSFontMetrics  dir ["Times-Roman"]
                          ; printLoadErrors metrics
                          ; makePictures metrics
                          }
      (_, Just dir) -> do { putStrLn "Using AFM v4.1 metrics..."
                          ; metrics <- loadAfmFontMetrics dir ["Times-Roman"]
                          ; printLoadErrors metrics
                          ; makePictures metrics
                          }
      _             -> putStrLn default_font_loader_help




makePictures :: FontLoadResult -> IO ()
makePictures base_metrics = do 
    let pic1 = runCtxPictureU (makeCtx 18 base_metrics) tree_drawing1
    writeEPS "./out/mon_tree01.eps"  pic1
    writeSVG "./out/mon_tree01.svg"  pic1

    let pic2 = runCtxPictureU (makeCtx 24 base_metrics) tree_drawing2
    writeEPS "./out/mon_tree02.eps"  pic2
    writeSVG "./out/mon_tree02.svg"  pic2


makeCtx :: FontSize -> FontLoadResult -> DrawingContext
makeCtx sz m = fontFace times_roman $ metricsContext sz m


-- Note - @label@ in TreeMonad is a waste of a valuable name...

tree1 :: TreeBuild u (TreeSpec Char)
tree1 = return $ 
    branch (label 'A') [branch (label 'B') bs, branch (label 'F') gs]
  where
    bs = [leaf $ label 'C', leaf $ label 'D', leaf $ label 'E']
    gs = [branch (label 'G') [ leaf $ label 'H'
                             , leaf $ label 'I'
                             , leaf $ label 'J' ] ]


tree_drawing1 :: DCtxPicture
tree_drawing1 = drawTracing $ 
    drawScaledTree (uniformSF 30) zeroPt $ runTreeBuild charNode tree1

-- Don\'t necessarily need @annotate@ from TreeMonad ...
--
nodeanno :: Num u 
         => DotLocImage u -> (DotAnchor u -> Point2 u) -> DotLocImage u
nodeanno img fn = annotate img deco
  where
    deco a = promoteR1 $ \_ -> let pt = fn a in textline "deco" `at` pt

tree2 :: (Real u, Floating u, FromPtSize u) => TreeBuild u (ZTreeSpec u)
tree2 = do
    special   <- nodeId $ nodeanno (dotText "a") south
    rightmost <- nodeId $ dotText "z"
    let bs = [zleaf, zleaf, zleaf]
    let gs = [zleaf, zleaf, leaf $ rightmost ]
    return $ 
        branch special [zbranch bs, zleaf, zbranch gs]




tree_drawing2 :: DCtxPicture
tree_drawing2 = drawTracing $ do
    draw $ textline "Tree 2" `at` zeroPt
    draw $ filledDisk 2      `at` displaceH (-40) tree_ogin 
    draw $ filledDisk 2      `at` tree_ogin
    draw $ filledDisk 2      `at` displaceH   40  tree_ogin
    drawScaledTreeD (uniformSF 60) tree_ogin TREE_RIGHT 
         $ runTreeBuild (diskNode red) tree2 
  where
    tree_ogin = P2 240 0

