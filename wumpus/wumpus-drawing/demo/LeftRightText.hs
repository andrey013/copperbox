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

import Wumpus.Drawing.Colour.SVGColours
import Wumpus.Drawing.Dots.Marks
import Wumpus.Drawing.Text.RotTextLR
import Wumpus.Drawing.Text.SafeFonts


import Wumpus.Basic.Kernel                      -- package: wumpus-basic
import Wumpus.Basic.System.FontLoader.Afm
import Wumpus.Basic.System.FontLoader.GhostScript

import Wumpus.Core                              -- package: wumpus-core

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
    base_metrics <- loadGSFontMetrics font_dir ["Helvetica"]
    printLoadErrors base_metrics
    let pic1 = runCtxPictureU (makeCtx base_metrics) text_pic
    writeEPS "./out/lr_text01.eps" pic1
    writeSVG "./out/lr_text01.svg" pic1

makeAfmPicture :: FilePath -> IO ()
makeAfmPicture font_dir = do
    putStrLn "Using AFM 4.1 metrics..."
    base_metrics <- loadAfmFontMetrics font_dir ["Helvetica"]
    printLoadErrors base_metrics
    let pic2 = runCtxPictureU (makeCtx base_metrics) text_pic
    writeEPS "./out/lr_text02.eps" pic2
    writeSVG "./out/lr_text02.svg" pic2




makeCtx :: FontLoadResult -> DrawingContext
makeCtx = set_font helvetica . metricsContext 15


text_pic :: CtxPicture
text_pic = udrawTracing (0::Double) $ do 
    drawi_ $ (fn left_text)       `at` P2   0 400
    drawi_ $ (fn center_text)     `at` P2 150 400
    drawi_ $ (fn right_text)      `at` P2 300 400
    drawi_ $ (fn blank_text)      `at` P2   0 300
    drawi_ $ (fn ne_oneline)      `at` P2 150 300 
    drawi_ $ (fn cc_oneline)      `at` P2 300 300


    drawi_ $ (fn sw_oneline)      `at` P2   0 200
    drawi_ $ (fn ss_oneline)      `at` P2 150 200
    drawi_ $ (fn se_oneline)      `at` P2 300 200
    drawi_ $ (fn swr_multi)       `at` P2   0 100
    drawi_ $ (fn ssr_multi)       `at` P2 150 100
    drawi_ $ (fn ner_multi)       `at` P2 300 100
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
   
redPlus :: (Fractional u, InterpretUnit u) => LocGraphic u
redPlus = localize (stroke_colour red) markPlus



-- single line
--
ne_oneline :: BoundedLocGraphic Double
ne_oneline = textbox "north east" `startPos` NE


-- single line
--
se_oneline :: BoundedLocGraphic Double
se_oneline = textbox "south east" `startPos` SE

-- single line
--
ss_oneline :: BoundedLocGraphic Double
ss_oneline = textbox "south" `startPos` SS

-- single line
--
sw_oneline :: BoundedLocGraphic Double
sw_oneline = textbox "south west" `startPos` SW


-- multi line
--
ssr_multi :: BoundedLocGraphic Double
ssr_multi = rotTextStart (multiAlignCenter "south rot45") SS (0.25*pi) 

-- multi line
--
swr_multi :: BoundedLocGraphic Double
swr_multi = rotTextStart (multiAlignCenter "south west rot45") SW (0.25*pi)

-- multi line
--
ner_multi :: BoundedLocGraphic Double
ner_multi = rotTextStart (multiAlignCenter "north east rot45") NE (0.25*pi)


cc_oneline :: BoundedLocGraphic Double
cc_oneline = rotTextStart (rtextbox "Center-center...") CENTER 0


blank_text :: BoundedLocGraphic Double
blank_text = rotTextStart (multiAlignCenter "") CENTER 0 


left_text :: BoundedLocGraphic Double
left_text = rotTextStart (multiAlignLeft dummy_text) CENTER 0


right_text :: BoundedLocGraphic Double
right_text = rotTextStart (multiAlignRight dummy_text) CENTER 0

center_text :: BoundedLocGraphic Double
center_text = rotTextStart (multiAlignCenter dummy_text) CENTER 0


rleft_text :: BoundedLocGraphic Double
rleft_text = rotTextStart (multiAlignLeft dummy_text) CENTER (0.25*pi)


rright_text :: BoundedLocGraphic Double
rright_text = rotTextStart (multiAlignRight dummy_text) CENTER (0.25*pi)

rcenter_text :: BoundedLocGraphic Double
rcenter_text = rotTextStart (multiAlignCenter dummy_text) CENTER (0.25*pi)


dummy_text :: String 
dummy_text = unlines $ [ "The quick brown"
                       , "fox jumps over"
                       , "the lazy dog."
                       ]
