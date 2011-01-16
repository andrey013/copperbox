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

import FontLoaderUtils

import Wumpus.Basic.Kernel
import Wumpus.Basic.System.FontLoader.Afm
import Wumpus.Basic.System.FontLoader.GhostScript
import Wumpus.Drawing.Colour.SVGColours
import Wumpus.Drawing.Dots.Marks
import Wumpus.Drawing.Text.LRText
import Wumpus.Drawing.Text.SafeFonts


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
    (gs_metrics, msgs) <- loadGSMetrics font_dir ["Helvetica"]
    mapM_ putStrLn msgs
    let pic1 = runCtxPictureU (makeCtx gs_metrics) text_pic
    writeEPS "./out/lr_text01.eps" pic1
    writeSVG "./out/lr_text01.svg" pic1

makeAfmPicture :: FilePath -> IO ()
makeAfmPicture font_dir = do
    putStrLn "Using AFM 4.1 metrics..."
    (afm_metrics, msgs) <- loadAfmMetrics font_dir ["Helvetica"]
    mapM_ putStrLn msgs
    let pic2 = runCtxPictureU (makeCtx afm_metrics) text_pic
    writeEPS "./out/lr_text02.eps" pic2
    writeSVG "./out/lr_text02.svg" pic2




makeCtx :: GlyphMetrics -> DrawingContext
makeCtx = fontFace helvetica . metricsContext 12


text_pic :: CtxPicture Double
text_pic = drawTracing $ do 
    drawi_ $ (fn left_text)       `at` P2   0 400
    drawi_ $ (fn center_text)     `at` P2 150 400
    drawi_ $ (fn right_text)      `at` P2 300 400
    drawi_ $ (fn blank_text)      `at` P2   0 300
    drawi_ $ (fn bl_oneline)      `at` P2 150 300
    drawi_ $ (fn cc_oneline)      `at` P2 300 300
    drawi_ $ (fn newblr)          `at` P2   0 200
    drawi_ $ (fn newblc)          `at` P2 150 200
    drawi_ $ (fn newbll)          `at` P2 300 200
    drawi_ $ (fn rnewblr)         `at` P2   0 100
    drawi_ $ (fn rnewblc)         `at` P2 150 100
    drawi_ $ (fn rnewbll)         `at` P2 300 100
    drawi_ $ (fn rleft_text)      `at` P2   0 (-75)
    drawi_ $ (fn rcenter_text)    `at` P2 150 (-75)
    drawi_ $ (fn rright_text)     `at` P2 300 (-75)

    draw $ redPlus            `at` P2   0 400
    draw $ redPlus            `at` P2 150 400
    draw $ redPlus            `at` P2 300 400
    draw $ redPlus            `at` P2   0 300  
    draw $ redPlus            `at` P2 150 300 
    draw $ redPlus            `at` P2 300 300 
    draw $ redPlus            `at` P2   0 200  
    draw $ redPlus            `at` P2 150 200 
    draw $ redPlus            `at` P2 300 200  
    draw $ redPlus            `at` P2   0 100  
    draw $ redPlus            `at` P2 150 100 
    draw $ redPlus            `at` P2 300 100 
    draw $ redPlus            `at` P2   0 (-75)
    draw $ redPlus            `at` P2 150 (-75)
    draw $ redPlus            `at` P2 300 (-75)
      
  where
    fn = illustrateBoundedLocGraphic
   
redPlus :: (Fractional u, FromPtSize u) => LocGraphic u
redPlus = localize (strokeColour red) markPlus

-- single line
--
newblc :: BoundedLocGraphic Double
newblc = singleLine BL_CENTER "new baseline center" `rot` 0

-- single line
--
newbll :: BoundedLocGraphic Double
newbll = singleLine BL_LEFT "new baseline left" `rot` 0

-- single line
--
newblr :: BoundedLocGraphic Double
newblr = singleLine BL_RIGHT "new baseline right" `rot` 0

-- single line
--
rnewblc :: BoundedLocGraphic Double
rnewblc = singleLine BL_CENTER "baseline center" `rot` (0.25*pi)


-- single line
--
rnewbll :: BoundedLocGraphic Double
rnewbll = singleLine BL_LEFT "baseline left" `rot` (0.25*pi)

-- single line
--
rnewblr :: BoundedLocGraphic Double
rnewblr = singleLine BL_RIGHT "baseline right" `rot` (0.25 * pi)


bl_oneline :: BoundedLocGraphic Double
bl_oneline = singleLine BL_LEFT "Baseline-left..." `rot` 0



cc_oneline :: BoundedLocGraphic Double
cc_oneline = singleLine CENTER "Center-center..."  `rot` 0

blank_text :: BoundedLocGraphic Double
blank_text = multiAlignCenter CENTER "" `rot` 0


left_text :: BoundedLocGraphic Double
left_text = multiAlignLeft CENTER dummy_text `rot` 0


right_text :: BoundedLocGraphic Double
right_text = multiAlignRight CENTER dummy_text `rot` 0

center_text :: BoundedLocGraphic Double
center_text = multiAlignCenter CENTER dummy_text `rot` 0


rleft_text :: BoundedLocGraphic Double
rleft_text = multiAlignLeft CENTER dummy_text `rot`   (0.25*pi)


rright_text :: BoundedLocGraphic Double
rright_text = multiAlignRight CENTER dummy_text `rot`  (0.25*pi)

rcenter_text :: BoundedLocGraphic Double
rcenter_text = multiAlignCenter CENTER dummy_text `rot` (0.25*pi)


dummy_text :: String 
dummy_text = unlines $ [ "The quick brown"
                       , "fox jumps over"
                       , "the lazy dog."
                       ]
