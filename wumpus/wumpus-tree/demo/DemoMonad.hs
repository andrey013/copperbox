

module DemoMonad where

import FontLoaderUtils

import Wumpus.Tree
import Wumpus.Tree.Base
import Wumpus.Tree.TreeBuildMonad

import Wumpus.Basic.Kernel                      -- package: wumpus-basic
import Wumpus.Basic.System.FontLoader.Afm
import Wumpus.Basic.System.FontLoader.GhostScript
import Wumpus.Drawing.Dots.AnchorDots
import Wumpus.Drawing.Colour.SVGColours
import Wumpus.Drawing.Text.SafeFonts

import Wumpus.Core                              -- package: wumpus-core

import Control.Monad
import Data.Tree

import System.Directory






tree1 :: TreeDrawing u (TreeSpec Char)
tree1 = return $ 
    branch (label 'A') [branch (label 'B') bs, branch (label 'F') gs]
  where
    bs = [leaf $ label 'C', leaf $ label 'D', leaf $ label 'E']
    gs = [branch (label 'G') [ leaf $ label 'H'
                             , leaf $ label 'I'
                             , leaf $ label 'J' ] ]


tree_drawing1 :: DTreePicture
tree_drawing1 = drawScaledTree2 (uniformScaling 30) $ 
                  runTreeDrawing charNode tree1



tree2 :: (Real u, Floating u, FromPtSize u) => TreeDrawing u ZTreeSpec
tree2 = do
    special <- nodeId $ dotText "root"   
    return $ 
      branch special [zbranch bs, zleaf, zbranch gs]
  where
    bs = [zleaf, zleaf, zleaf]
    gs = [zleaf, zleaf, zleaf]



tree_drawing2 :: DTreePicture
tree_drawing2 = drawScaledTree2 (uniformScaling 60) $ 
                  runTreeDrawing (diskNode red) 
                                 tree2 


main :: IO ()
main = do
    (mb_gs, mb_afm) <- processCmdLine default_font_loader_help
    createDirectoryIfMissing True "./out/"
    case (mb_gs, mb_afm) of       
      (Just dir, _) -> do { putStrLn "Using GhostScript metrics..."
                          ; (metrics,msgs) <- loadGSMetrics  dir ["Times-Roman"]
                          ; mapM_ putStrLn msgs
                          ; makePictures metrics
                          }
      (_, Just dir) -> do { putStrLn "Using AFM v4.1 metrics..."
                          ; (metrics,msgs) <- loadAfmMetrics dir ["Times-Roman"]
                          ; mapM_ putStrLn msgs
                          ; makePictures metrics
                          }
      _             -> putStrLn default_font_loader_help




makePictures :: GlyphMetrics -> IO ()
makePictures base_metrics = do 
    let pic1 = runCtxPictureU (makeCtx 18 base_metrics) tree_drawing1
    writeEPS "./out/mon_tree01.eps"  pic1
    writeSVG "./out/mon_tree01.svg"  pic1

    let pic2 = runCtxPictureU (makeCtx 24 base_metrics) tree_drawing2
    writeEPS "./out/mon_tree02.eps"  pic2
    writeSVG "./out/mon_tree02.svg"  pic2


makeCtx :: FontSize -> GlyphMetrics -> DrawingContext
makeCtx sz m = fontFace times_roman $ metricsContext sz m

