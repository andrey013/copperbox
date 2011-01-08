{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}


module TempShapes where

import Wumpus.Basic.Kernel
import Wumpus.Basic.System.FontLoader.GhostScript
import Wumpus.Drawing.Arrows
import Wumpus.Drawing.Paths
import Wumpus.Drawing.Shapes
import Wumpus.Drawing.Text.LRText
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
    (base_metrics,msgs) <- loadGSMetrics font_directory ["Courier"]
    mapM_ putStrLn msgs
    let pic1 = runCtxPictureU (makeCtx base_metrics) shapes_pic
    writeEPS "./out/_temp_shapes01.eps" pic1
    writeSVG "./out/_temp_shapes01.svg" pic1
    

makeCtx :: GlyphMetrics -> DrawingContext
makeCtx = fontFace courier . metricsContext 14


shapes_pic :: DCtxPicture
shapes_pic = drawTracing $ do
    drawi_ $ borderedShape $ rectangle 60  30 $ zeroPt
    drawi_ $ ctrCenterLine "Rect1" `at` zeroPt
    a0 <- drawi $ borderedShape $ rrectangle 10 60 30 $ P2 80 0
    drawi_ $ borderedShape $ circle 40 $ P2 160 0
    drawi_ $ filledShape   $ rdiamond 5 10 20 $ P2 220 0
    a1 <- drawi $ strokedShape  $ ellipse 20 10 $ P2 0 80
    drawi_ $ coordinateDot $ coordinate (center a1)
    connectRA (north a0) (southeast a1)


connectRA :: ( TraceM m, DrawingCtxM m, u ~ MonUnit m
             , Real u, Floating u, FromPtSize u ) 
          => Point2 u -> Point2 u -> m ()
connectRA p0 p1 = 
    drawi_ $ apply2R2 (strokeConnector (rightArrow connLine barb45)) p0 p1
