{-# OPTIONS -Wall #-}

module Demo01 where

import Wumpus.MicroPrint
import Wumpus.MicroPrint.DrawMonad
import Wumpus.MicroPrint.Render

import Wumpus.Core
import Wumpus.Extra.SVGColours

import Data.Maybe
import System.Directory

main :: IO ()
main = createDirectoryIfMissing True "./out/"
    >> writeEPS_latin1 "./out/mp01.eps" pic1
    >> writeSVG_latin1 "./out/mp01.svg" pic1


pic1 :: Picture Double
pic1 = fromMaybe errK $ drawMicroPrint cfg1 mp1

mp1 :: ([Tile],Height)
mp1 = execTrace (setRGB lightSlateGrey >> char >> space >> space >> char >> char
                >> linebreak >> setRGB firebrick >> char >> char >> char >> char)

errK :: a
errK = error "no picture"



cfg1 :: MP_config
cfg1 = MP_config 
       { char_height    = 12.0
       , char_width     = 8.0
       , line_spacing   = 3.0
       , drawF          = greekF
       }
 

