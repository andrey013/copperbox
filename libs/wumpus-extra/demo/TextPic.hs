{-# OPTIONS -Wall #-}

module TextPic where

import Wumpus.Core
import Wumpus.Extra
import Wumpus.Extra.PictureLanguage
import Wumpus.Extra.SafeFonts
import Wumpus.Extra.SVGColours

import Ssytem.Directory

main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/"
    demo1


demo01 :: IO ()
demo01 = do 
    writeEPS_latin1 "./out/text_pic01.eps" text_pic01
    writeSVG_latin1 "./out/text_pic01.svg" text_pic01 


all_nums, all_uppers, all_lowers :: String
all_nums   = "0123456789"
all_uppers = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
all_lowers = "abcdefghijklmnopqrstuvwxyz"

text_pic01 :: Picture Double
text_pic01 = pic1 -//- pic2
 

pic1 :: Picture Double
pic1 = vsepA VLeft 4 line1 [line2,line3]
  where
    line1   = frame $ textlabel courier24 all_nums   zeroPt
    line2   = frame $ textlabel courier24 all_uppers zeroPt
    line3   = frame $ textlabel courier24 all_lowers zeroPt


pic2 :: Picture Double
pic2 = backgroundFill lightCoral $ textline courier24 "Wumpus" zeroPt

