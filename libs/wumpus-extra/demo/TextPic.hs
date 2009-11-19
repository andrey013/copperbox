{-# OPTIONS -Wall #-}

module TextPicture where

import Wumpus.Core
import Wumpus.Extra
import Wumpus.Extra.SVGColours



main :: IO ()
main = sequence_ [ demo01 ]



demo01 :: IO ()
demo01 = do 
    writeEPS "./out/text_pic01.eps" Nothing text_pic01
    writeSVG "./out/text_pic01.svg" text_pic01 


all_nums, all_uppers, all_lowers :: String
all_nums   = "0123456789"
all_uppers = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
all_lowers = "abcdefghijklmnopqrstuvwxyz"

text_pic01 :: Picture Double
text_pic01 = pic1 -//- pic2
 

pic1 :: Picture Double
pic1 = vsepA VLeft 4 line1 [line2,line3]
  where
    line1   = frame $ textlabel courier24 zeroPt all_nums
    line2   = frame $ textlabel courier24 zeroPt all_uppers
    line3   = frame $ textlabel courier24 zeroPt all_lowers


pic2 :: Picture Double
pic2 = backgroundFill lightCoral $ textline courier24 zeroPt "Wumpus"

