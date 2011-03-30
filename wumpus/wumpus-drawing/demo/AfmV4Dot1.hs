{-# OPTIONS -Wall #-}


module AfmV4Dot1 where


import Wumpus.Drawing.Colour.SVGColours
import Wumpus.Drawing.Dots.Marks
import Wumpus.Drawing.Text.DirectionZero
import Wumpus.Drawing.Text.StandardFontDefs

import Wumpus.Basic.Kernel                      -- package: wumpus-basic
import Wumpus.Basic.System.FontLoader.AfmTopLevel

import Wumpus.Core                              -- package: wumpus-core

import System.Directory



-- Edit this path!
-- ***************
--
font_directory :: FilePath
font_directory = "D:/coding/haskell/GHC_workspace/wumpus/_font_metrics/adobe_core14"



main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/"
    base_metrics <- loadAfmFontMetrics font_directory [ helvetica, times_roman ]
    printLoadErrors base_metrics
    let pic1 = runCtxPictureU (makeCtx base_metrics) text_pic
    writeEPS "./out/afm4dot1.eps" pic1
    writeSVG "./out/afm4dot1.svg" pic1


makeCtx :: FontLoadResult -> DrawingContext
makeCtx = set_font helvetica . metricsContext 18


text_pic :: CtxPicture
text_pic = drawTracing $ do 
    draw $ (fn left_text)   `at` P2   0 100
    draw $ (fn center_text) `at` P2 250 100
    draw $ (fn right_text)  `at` P2 500 100
    draw $ (fn blank_text)  `at` P2   0   0
    draw $ (fn bl_oneline)  `at` P2 250   0
    draw $ (fn cc_oneline)  `at` P2 500   0
    
    draw $ redPlus            `at` P2   0 100
    draw $ redPlus            `at` P2 250 100
    draw $ redPlus            `at` P2 500 100
    draw $ redPlus            `at` P2   0   0  
    draw $ redPlus            `at` P2 250   0 
    draw $ redPlus            `at` P2 500   0 
       
  where
    fn = illustrateBoundedLocGraphic

redPlus :: (Fractional u, InterpretUnit u) => LocGraphic u
redPlus = localize (stroke_colour red) markPlus

bl_oneline :: BoundedLocGraphic Double
bl_oneline = 
    localize (set_font times_roman . stroke_colour dark_slate_gray) $ 
             bllTextline "Times-Roman"


cc_oneline :: BoundedLocGraphic Double
cc_oneline = 
    localize (stroke_colour dark_slate_gray) $ ccTextline "Center-center..."

blank_text :: BoundedLocGraphic Double
blank_text = 
    localize (stroke_colour dark_slate_gray) $ bllTextline ""


left_text :: BoundedLocGraphic Double
left_text = 
    localize (stroke_colour dark_slate_gray) $ ccTextline dummy_text


right_text :: BoundedLocGraphic Double
right_text = 
    localize (stroke_colour dark_slate_gray) $ 
       textline dummy_text `startAddr` BLR

center_text :: BoundedLocGraphic Double
center_text = 
    localize (stroke_colour dark_slate_gray) $ blcTextline dummy_text


dummy_text :: String 
dummy_text = unlines $ [ "Using Afm V4.1"
                       , "metrics..."
                       ]
