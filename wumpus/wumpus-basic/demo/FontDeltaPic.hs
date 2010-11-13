{-# OPTIONS -Wall #-}

module FontDeltaPic where

import Wumpus.Basic.Graphic

import Wumpus.Core                      -- package: wumpus-core

import System.Directory

main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/"
    demo01

pt2 :: Point2 Double
pt2 = P2 100 10


demo01 :: IO ()
demo01 = do 
    writeEPS "./out/font_delta01.eps" pic1
    writeSVG "./out/font_delta01.svg" pic1


std_attr :: DrawingContext
std_attr = standardContext 24


pic1 :: DPicture
pic1 = liftToPictureU $ execTraceDrawing std_attr $ mf 

errK :: a
errK = error "error - empty Picture"

mf :: (Floating u, FromPtSize u) => TraceDrawing u ()
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


-- TODO - this should use a chain...


