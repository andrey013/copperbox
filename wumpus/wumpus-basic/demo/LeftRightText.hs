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


import Wumpus.Basic.Kernel
import Wumpus.Basic.System.GSLoader
import Wumpus.Basic.System.AfmLoader
import Wumpus.Drawing.Colour.SVGColours
import Wumpus.Drawing.Dots.Marks
import Wumpus.Drawing.Text.LRText
import Wumpus.Drawing.Text.SafeFonts

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
    writeEPS "./out/lr_text01.eps" pic1
    writeSVG "./out/lr_text01.svg" pic1

makeAfmPicture :: FilePath -> IO ()
makeAfmPicture font_dir = do
    putStrLn "Using AFM 4.1 metrics..."
    afm_metrics <- loadAfmMetrics font_dir ["Helvetica"]
    let pic2 = runDrawingU (makeCtx afm_metrics) text_drawing 
    writeEPS "./out/lr_text02.eps" pic2
    writeSVG "./out/lr_text02.svg" pic2




makeCtx :: BaseGlyphMetrics -> DrawingContext
makeCtx = fontFace helvetica . metricsContext 18


text_drawing :: Drawing Double
text_drawing = drawTracing $ do 
    drawi_ $ (fn left_text)   `at` P2   0 300
    drawi_ $ (fn center_text) `at` P2 200 300
    drawi_ $ (fn right_text)  `at` P2 400 300
    drawi_ $ (fn blank_text)  `at` P2   0 200
    drawi_ $ (fn bl_oneline)  `at` P2 200 200
    drawi_ $ (fn cc_oneline)  `at` P2 400 200
    drawi_ $ (fn $ newblr)    `at` P2   0 100
    drawi_ $ (fn $ newblc)    `at` P2 200 100
    drawi_ $ (fn $ newbll)    `at` P2 400 100
    drawi_ $ (fn $ rnewblr)   `at` P2   0   0
    drawi_ $ (fn $ rnewblc)   `at` P2 200   0
    drawi_ $ (fn $ rnewbll)   `at` P2 400   0
      
 
    draw $ redPlus            `at` P2   0 300
    draw $ redPlus            `at` P2 200 300
    draw $ redPlus            `at` P2 400 300
    draw $ redPlus            `at` P2   0 200  
    draw $ redPlus            `at` P2 200 200 
    draw $ redPlus            `at` P2 400 200 
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


newblc :: BoundedLocGraphic Double
newblc = 
    localize (strokeColour dark_slate_gray) $ 
        baseCenterLine "new baseline center" `rot` 0

newbll :: BoundedLocGraphic Double
newbll = 
    localize (strokeColour dark_slate_gray) $ 
        baseLeftLine "new baseline left" `rot` 0

newblr :: BoundedLocGraphic Double
newblr = 
    localize (strokeColour dark_slate_gray) $ 
        baseRightLine "new baseline right" `rot` 0


rnewblc :: BoundedLocGraphic Double
rnewblc = 
    localize (strokeColour dark_slate_gray) $ 
        baseCenterLine "baseline center" `rot` (0.25*pi)

rnewbll :: BoundedLocGraphic Double
rnewbll = 
    localize (strokeColour dark_slate_gray) $ 
        baseLeftLine "baseline left" `rot` (0.25*pi)

rnewblr :: BoundedLocGraphic Double
rnewblr = 
    localize (strokeColour dark_slate_gray) $ 
        baseRightLine "baseline right" `rot` (0.25 * pi)


bl_oneline :: BoundedLocGraphic Double
bl_oneline = 
    localize (strokeColour dark_slate_gray) $ baseLeftLine "Baseline-left..." `rot` 0


cc_oneline :: BoundedLocGraphic Double
cc_oneline = 
    localize (strokeColour dark_slate_gray) $ ctrCenterLine "Center-center..."

blank_text :: BoundedLocGraphic Double
blank_text = 
    localize (strokeColour dark_slate_gray) $ multiAlignCenter "" `rot` 0


left_text :: BoundedLocGraphic Double
left_text = 
    localize (strokeColour dark_slate_gray) $ multiAlignLeft dummy_text `rot` 0


right_text :: BoundedLocGraphic Double
right_text = 
    localize (strokeColour dark_slate_gray) $ multiAlignRight dummy_text `rot` 0

center_text :: BoundedLocGraphic Double
center_text = 
    localize (strokeColour dark_slate_gray) $ multiAlignCenter dummy_text `rot` 0


dummy_text :: String 
dummy_text = unlines $ [ "The quick brown"
                       , "fox jumps over"
                       , "the lazy dog."
                       ]
