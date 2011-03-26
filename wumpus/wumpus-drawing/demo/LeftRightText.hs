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


import Wumpus.Drawing.Colour.SVGColours
import Wumpus.Drawing.Dots.Marks
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
    base_metrics <- loader [helvetica]
    printLoadErrors base_metrics
    let pic1 = runCtxPictureU (makeCtx base_metrics) text_pic
    writeEPS "./out/left_right_text.eps" pic1
    writeSVG "./out/left_right_text.svg" pic1



makeCtx :: FontLoadResult -> DrawingContext
makeCtx = set_font helvetica . metricsContext 15


text_pic :: CtxPicture
text_pic = udrawTracing (0::Double) $ localize text_margin_tight $ do 
    draw $ (fn left_text)       `at` P2   0 400
    draw $ (fn center_text)     `at` P2 150 400
    draw $ (fn right_text)      `at` P2 300 400
    draw $ (fn blank_text)      `at` P2   0 300
    draw $ (fn ne_oneline)      `at` P2 150 300 
    draw $ (fn cc_oneline)      `at` P2 300 300


    draw $ (fn sw_oneline)      `at` P2   0 200
    draw $ (fn ss_oneline)      `at` P2 150 200
    draw $ (fn se_oneline)      `at` P2 300 200
    draw $ (fn swr_multi)       `at` P2   0 100
    draw $ (fn ssr_multi)       `at` P2 150 100
    draw $ (fn ner_multi)       `at` P2 300 100
    draw $ (fn rleft_text)      `at` P2   0 (-75)
    draw $ (fn rcenter_text)    `at` P2 150 (-75)
    draw $ (fn rright_text)     `at` P2 300 (-75)

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
