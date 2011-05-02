{-# OPTIONS -Wall #-}


module DemoMonad where

import Wumpus.Tree.TreeBuilder
import Wumpus.Tree

import Wumpus.Drawing.Colour.SVGColours         -- package: wumpus-drawing
import Wumpus.Drawing.Connectors
import Wumpus.Drawing.Dots.AnchorDots
import Wumpus.Drawing.Text.StandardFontDefs

import Wumpus.Basic.Kernel                      -- package: wumpus-basic
import Wumpus.Basic.System.FontLoader

import Wumpus.Core                              -- package: wumpus-core


import qualified Data.IntMap as IntMap
import System.Directory


main :: IO ()
main = simpleFontLoader main1 >> return ()

main1 :: FontLoader -> IO ()
main1 loader = do
    createDirectoryIfMissing True "./out/" 
    base_metrics <- loader [ Right times_roman_family ]
    printLoadErrors base_metrics
    let pic1 = runCtxPictureU (makeCtx 24 base_metrics) ctx_pic1
    writeEPS "./out/demo_monad01.eps"  pic1
    writeSVG "./out/demo_monad01.svg"  pic1


makeCtx :: FontSize -> FontLoadResult -> DrawingContext
makeCtx sz m = set_font times_roman $ metricsContext sz m



ctx_pic1 :: CtxPicture
ctx_pic1 = udrawTracing (0::Double) $ do
    --
    
    drawTreeSpec props1 (P2 100 100) spec1
    return ()
  where
    props1 = standardTreeProps 50 70 familyOTMC



spec1 :: (Real u, Floating u, Tolerance u, InterpretUnit u) 
      => AbsTreeSpec u (DotAnchor u)
spec1 = do
    a <- ref dotDisk
    b <- ref dotDisk
    linkref a b (\a b -> graphic_ $ 
                   connect (uniformArrow curveTip connhbezier) (west a) (east b))
    return (root a [leaf b,  mkleaf dotCircle])


