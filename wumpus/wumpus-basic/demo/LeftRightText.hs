{-# OPTIONS -Wall #-}

-- Note - @main@ is more convoluted than would normally be 
-- expected as it supports both sources of glyph metrics - the 
-- GhostScript distribution or the Core 14 metrics from Adobe.
-- 
-- \"Real\" applications would be expected to choose one source. 
--
-- I-am-not-a-lawyer, but it does look as though the Adobe font
-- metrics are redistributable, the GhostScript metrics are 
-- seemingly redistributable under the same terms as the larger
-- GhostScript distribution.
-- 


module LeftRightText where


import Wumpus.Basic.Colour.SVGColours
import Wumpus.Basic.Dots.Marks
import Wumpus.Basic.FontLoader.GSLoader
import Wumpus.Basic.FontLoader.AfmLoader
import Wumpus.Basic.Graphic
import Wumpus.Basic.SafeFonts
import Wumpus.Basic.Text.LRText

import FontLoaderUtils


import Wumpus.Core                      -- package: wumpus-core

import System.Directory



main :: IO ()
main = do 
    (mb_gs, mb_afm) <- processCmdLine default_font_loader_help
    createDirectoryIfMissing True "./out/"
    maybe gs_failk  makeGSPicture  $ mb_gs
    maybe afm_failk makeAfmPicture $ mb_afm
  where
    gs_failk  = putStrLn "No GhostScript font path supplied..."
    afm_failk = putStrLn "No AFM v4.1 font path supplied..."


makeGSPicture :: FilePath -> IO ()
makeGSPicture font_dir = do
    putStrLn "Using GhostScript metrics..."
    gs_metrics <- loadGSMetrics font_dir ["Helvetica"]
    let pic1 = runDrawingU (makeCtx gs_metrics) text_drawing 
    writeEPS "./out/new_text01.eps" pic1
    writeSVG "./out/new_text01.svg" pic1

makeAfmPicture :: FilePath -> IO ()
makeAfmPicture font_dir = do
    putStrLn "Using AFM 4.1 metrics..."
    afm_metrics <- loadAfmMetrics font_dir ["Helvetica"]
    let pic2 = runDrawingU (makeCtx afm_metrics) text_drawing 
    writeEPS "./out/new_text02.eps" pic2
    writeSVG "./out/new_text02.svg" pic2




makeCtx :: BaseGlyphMetrics -> DrawingContext
makeCtx = fontface helvetica . metricsContext 18


text_drawing :: Drawing Double
text_drawing = drawTracing $ do 
    drawi_ $ (fn left_text)   `at` P2   0 100
    drawi_ $ (fn center_text) `at` P2 200 100
    drawi_ $ (fn right_text)  `at` P2 400 100
    drawi_ $ (fn blank_text)  `at` P2   0   0
    drawi_ $ (fn bl_oneline)  `at` P2 200   0
    drawi_ $ (fn cc_oneline)  `at` P2 400   0
    
    draw $ redPlus            `at` P2   0 100
    draw $ redPlus            `at` P2 200 100
    draw $ redPlus            `at` P2 400 100
    draw $ redPlus            `at` P2   0   0  
    draw $ redPlus            `at` P2 200   0 
    draw $ redPlus            `at` P2 400   0 
       
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
dummy_text = unlines $ [ "The quick brown"
                       , "fox jumps over"
                       , "the lazy dog."
                       ]
