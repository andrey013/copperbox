{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}


module TempShapes where

import Wumpus.Basic.Kernel
import Wumpus.Basic.System.FontLoader.GhostScript
import Wumpus.Drawing.Arrows
import Wumpus.Drawing.Paths
import Wumpus.Drawing.Shapes
import Wumpus.Drawing.Shapes.Triangle
import Wumpus.Drawing.Text.RotTextLR
import Wumpus.Drawing.Text.SafeFonts

import Wumpus.Core                              -- package: wumpus-core


import System.Directory

-- Edit this path!
-- ***************
--
font_directory :: FilePath
font_directory = "C:/cygwin/usr/share/ghostscript/fonts"


main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/"
    base_metrics <- loadGSFontMetrics font_directory ["Courier"]
    printLoadErrors base_metrics
    let pic1 = runCtxPictureU (makeCtx base_metrics) shapes_pic
    writeEPS "./out/_temp_shapes01.eps" pic1
    writeSVG "./out/_temp_shapes01.svg" pic1
    

makeCtx :: FontLoadResult -> DrawingContext
makeCtx = set_font courier . metricsContext 14


shapes_pic :: DCtxPicture
shapes_pic = drawTracing $ do
    drawi_ $ (borderedShape $ rectangle 60 30) `at` zeroPt
    drawi_ $ textAlignCenter "Rect1" `at` zeroPt
    a0 <- drawi $ (borderedShape $ rectangle 60 30) `at` P2 80 0
    drawi_ $ (borderedShape $ circle 40) `at` P2 160 0
    drawi_ $ (filledShape   $ diamond 10 20) `at` P2 220 0
    a1 <- drawi $ (strokedShape  $ ellipse 20 10) `at` P2 0 80
--    drawi_ $ coordinateDot $ coordinate (center a1)
    connectRA (north a0) (southeast a1)
    a2  <- drawi $ (strokedShape $ triangle 40 20) `at` P2 100 80
    
    return ()    


connectRA :: ( TraceM m, DrawingCtxM m, u ~ DUnit (m ())
             , Real u, Floating u, PtSize u) 
          => Point2 u -> Point2 u -> m ()
connectRA p0 p1 = 
    drawi_ $ apply2R2 (rightArrow barb45 connLine) p0 p1
