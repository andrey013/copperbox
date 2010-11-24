{-# OPTIONS -Wall #-}


module NewText where


import Wumpus.Basic.Colour.SVGColours
import Wumpus.Basic.Dots.Marks
import Wumpus.Basic.FontLoader.AfmV2
import Wumpus.Basic.FontLoader.Base
import Wumpus.Basic.Graphic
import Wumpus.Basic.SafeFonts
import Wumpus.Basic.Text.LRText

import Wumpus.Core                      -- package: wumpus-core

import System.Directory



-- Edit this path!
-- ***************
--
font_directory :: FilePath
font_directory = "C:/cygwin/usr/share/ghostscript/fonts"


main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/"
    base_metrics <- loadBaseGlyphMetrics loader ["Helvetica"]
    let pic1 = runDrawingU (makeStdCtx base_metrics) text_drawing 
    writeEPS "./out/new_text01.eps" pic1
    writeSVG "./out/new_text01.svg" pic1
  where
    loader = ghostScriptFontLoader font_directory

makeStdCtx :: BaseGlyphMetrics -> DrawingContext
makeStdCtx = fontface helvetica . metricsContext 18


text_drawing :: Drawing Double
text_drawing = drawTracing $ do 
    drawi_ $ (fn left_text)   `at` zeroPt
    drawi_ $ (fn center_text) `at` P2 250 0
    drawi_ $ (fn right_text)  `at` P2 500 0
    drawi_ $ (fn blank_text)  `at` P2 0   (-100)
    
    draw $ redPlus            `at` zeroPt
    draw $ redPlus            `at` P2 250 0
    draw $ redPlus            `at` P2 500 0
        
  where
    fn = illustrateBoundedLocGraphic

redPlus :: (Fractional u, FromPtSize u) => LocGraphic u
redPlus = localize (strokeColour red) markPlus


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
dummy_text = unlines $ [ "The quick brown"
                       , "fox jumps over"
                       , "the lazy dog."
                       ]
