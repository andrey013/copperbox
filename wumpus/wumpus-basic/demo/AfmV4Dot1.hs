{-# OPTIONS -Wall #-}


module AfmV4Dot1 where


import Wumpus.Basic.Colour.SVGColours
import Wumpus.Basic.Dots.Marks
import Wumpus.Basic.FontLoader.AfmV4Dot1
import Wumpus.Basic.Graphic
import Wumpus.Basic.SafeFonts
import Wumpus.Basic.Text.LRText

import Wumpus.Core                      -- package: wumpus-core

import System.Directory



-- Edit this path!
-- ***************
--
font_directory :: FilePath
font_directory = "./font_metrics/adobe_core14"



main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/"
    base_metrics <- loadBaseGlyphMetrics loader ["Helvetica"]
    let pic1 = runDrawingU (makeCtx base_metrics) text_drawing 
    writeEPS "./out/afm4dot1_01.eps" pic1
    writeSVG "./out/afm4dot1_01.svg" pic1


loader :: FontLoader AfmUnit
loader = afmV4Dot1Loader font_directory

makeCtx :: BaseGlyphMetrics -> DrawingContext
makeCtx = fontface helvetica . metricsContext 18


text_drawing :: Drawing Double
text_drawing = drawTracing $ do 
    drawi_ $ (fn left_text)   `at` P2   0 100
    drawi_ $ (fn center_text) `at` P2 250 100
    drawi_ $ (fn right_text)  `at` P2 500 100
    drawi_ $ (fn blank_text)  `at` P2   0   0
    drawi_ $ (fn bl_oneline)  `at` P2 250   0
    drawi_ $ (fn cc_oneline)  `at` P2 500   0
    
    draw $ redPlus            `at` P2   0 100
    draw $ redPlus            `at` P2 250 100
    draw $ redPlus            `at` P2 500 100
    draw $ redPlus            `at` P2   0   0  
    draw $ redPlus            `at` P2 250   0 
    draw $ redPlus            `at` P2 500   0 
       
  where
    fn = illustrateBoundedLocGraphic

redPlus :: (Fractional u, FromPtSize u) => LocGraphic u
redPlus = localize (strokeColour red) markPlus

bl_oneline :: BoundedLocGraphic Double
bl_oneline = 
    localize (strokeColour dark_slate_gray) $ singleLineBL "Baseline-left..."


cc_oneline :: BoundedLocGraphic Double
cc_oneline = 
    localize (strokeColour dark_slate_gray) $ singleLineCC "Center-center..."

blank_text :: BoundedLocGraphic Double
blank_text = 
    localize (strokeColour dark_slate_gray) $ multiAlignCenter ""


left_text :: BoundedLocGraphic Double
left_text = 
    localize (strokeColour dark_slate_gray) $ multiAlignLeft dummy_text


right_text :: BoundedLocGraphic Double
right_text = 
    localize (strokeColour dark_slate_gray) $ multiAlignRight dummy_text

center_text :: BoundedLocGraphic Double
center_text = 
    localize (strokeColour dark_slate_gray) $ multiAlignCenter dummy_text


dummy_text :: String 
dummy_text = unlines $ [ "Using Afm V4.1"
                       , "metrics..."
                       ]
