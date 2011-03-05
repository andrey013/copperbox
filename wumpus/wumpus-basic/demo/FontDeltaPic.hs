{-# OPTIONS -Wall #-}

-- Note - this demo is not really exemplary - it is only here
-- to check the compilation of Wumpus-Basic. There are more 
-- impressive demos in the @Wumpus-Drawing@ package.
-- 

module FontDeltaPic where

import Wumpus.Basic.Kernel

import Wumpus.Core                      -- package: wumpus-core

import System.Directory

main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/"
    putStrLn $ "Note - the SVG optimization that should be used here"
    putStrLn $ "has bit-rotted and is not currently in use.."
    --
    let pic1 = runCtxPictureU std_attr drawing01
    writeEPS "./out/font_delta01.eps" pic1
    writeSVG "./out/font_delta01.svg" pic1


std_attr :: DrawingContext
std_attr = standardContext 24


drawing01 :: CtxPicture
drawing01 = drawTracing UDouble $ mf 


mf :: TraceDrawing Double ()
mf = do 
    draw $ line1 `at` (P2 0 100)
    draw $ line2 `at` (P2 0  75)
    draw $ line3 `at` (P2 0  50)
    draw $ line4 `at` (P2 0  25)
    draw $ line5 `at` (P2 0   0) 
  where
    line1 = textline "All the lines of this drawing" 
    line2 = textline "should be grouped within a SVG"
    line3 = textline "g-element, from where they"
    line4 = textline "inherit the font-family and"
    line5 = textline "font-size attributes."




