{-# OPTIONS -Wall #-}



module LeftRightText where


import Wumpus.Drawing.Colour.SVGColours
import Wumpus.Drawing.Dots.SimpleDots
import Wumpus.Drawing.Text.DirectionZero
import Wumpus.Drawing.Text.StandardFontDefs

import Wumpus.Basic.Kernel                      -- package: wumpus-basic
import Wumpus.Basic.System.FontLoader

import Wumpus.Core                              -- package: wumpus-core

import System.Directory


main :: IO ()
main = simpleFontLoader main1 >> return ()

main1 :: FontLoader -> IO ()
main1 loader = do
    createDirectoryIfMissing True "./out/" 
    base_metrics <- loader [ Left helvetica ]
    printLoadErrors base_metrics
    let pic1 = runCtxPictureU (makeCtx base_metrics) text_pic
    writeEPS "./out/left_right_text.eps" pic1
    writeSVG "./out/left_right_text.svg" pic1



makeCtx :: FontLoadResult -> DrawingContext
makeCtx = set_font helvetica . metricsContext 15


text_pic :: CtxPicture
text_pic = udrawTracing (0::Double) $ localize text_margin_loose $ do 
    draw $ (fn left_text)       `at` P2   0 400
    draw $ (fn center_text)     `at` P2 150 400
    draw $ (fn right_text)      `at` P2 300 400
    draw $ (fn blank_text)      `at` P2   0 300
    draw $ (fn ne_oneline)      `at` P2 150 300 
    draw $ (fn cc_oneline)      `at` P2 300 300


    draw $ (fn sw_oneline)      `at` P2   0 200
    draw $ (fn ss_oneline)      `at` P2 150 200
    draw $ (fn se_oneline)      `at` P2 300 200
    draw $ (fn swr_single)      `at` P2   0 100
    draw $ (fn ssr_single)      `at` P2 150 100
    draw $ (fn ner_single)      `at` P2 300 100

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
redPlus = localize (stroke_colour red) dotPlus



-- single line
--
ne_oneline :: BoundedLocGraphic Double
ne_oneline = textline "north east" NE


-- single line
--
se_oneline :: BoundedLocGraphic Double
se_oneline = textline "south east" SE

-- single line
--
ss_oneline :: BoundedLocGraphic Double
ss_oneline = textline "south" SS

-- single line
--
sw_oneline :: BoundedLocGraphic Double
sw_oneline = textline "south west" SW



-- single line rot
--
ssr_single :: BoundedLocGraphic Double
ssr_single = rtextline (0.25*pi) "south rot45"  SS

-- single line rot
--
swr_single :: BoundedLocGraphic Double
swr_single = rtextline (0.25*pi)  "south west rot45" SW

-- single line rot
--
ner_single :: BoundedLocGraphic Double
ner_single = rtextline (0.25*pi)  "north east rot45" NE


cc_oneline :: BoundedLocGraphic Double
cc_oneline = ccTextline "Center-center..."


blank_text :: BoundedLocGraphic Double
blank_text = blcTextline ""


left_text :: BoundedLocGraphic Double
left_text = multiAlignLeft dummy_text CENTER


right_text :: BoundedLocGraphic Double
right_text = multiAlignRight dummy_text CENTER

center_text :: BoundedLocGraphic Double
center_text = multiAlignCenter dummy_text CENTER

dummy_text :: String 
dummy_text = unlines $ [ "The quick brown"
                       , "fox jumps over"
                       , "the lazy dog."
                       ]
