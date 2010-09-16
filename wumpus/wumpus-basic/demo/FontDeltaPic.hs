{-# OPTIONS -Wall #-}

module FontDeltaPic where

import Wumpus.Basic.Graphic

import Wumpus.Core                      -- package: wumpus-core


import Data.Maybe
import System.Directory

main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/"
    demo01

pt2 :: Point2 Double
pt2 = P2 100 10


demo01 :: IO ()
demo01 = do 
    writeEPS_latin1 "./out/font_delta01.eps" pic1
    writeSVG_latin1 "./out/font_delta01.svg" pic1


std_attr :: DrawingContext
std_attr = standardContext 24


pic1 :: DPicture
pic1 = fromMaybe errK $ execFdcDrawing std_attr $ mf 

errK :: a
errK = error "error - empty Picture"

mf :: (Floating u, FromPtSize u) => Drawing u ()
mf = do 
    draw $ line1 `at` (P2 0  50)
    draw $ line2 `at` (P2 0  25)
    draw $ line3 `at` (P2 0  0)
    
  where
    line1 = textline "All the lines of this drawing should be grouped"
    line2 = textline "within a SVG g-element, from where they inherit"
    line3 = textline "the font-family and font-size attributes."


-- Note - lines are drawn in the reverse order (3 2 1) in both 
-- PS and SVG output. Maybe the drawing order in the Draw monad 
-- should be top to bottom after all...

-- TODO - this should have shorter lines (too big for PS at the 
-- moment) and use a chain...


