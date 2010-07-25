{-# OPTIONS -Wall #-}

module Timing1 where

import Wumpus.Timing.Alphabet
import Wumpus.Timing.Drawing
import Wumpus.Timing.Width


import Wumpus.Core                      -- package: wumpus-core
import Wumpus.Basic.Graphic             -- package: wumpus-basic
import Wumpus.Basic.SVGColours

import System.Directory

import Data.List

main :: IO ()
main = createDirectoryIfMissing True "./out/"
    >> writeEPS_latin1 "./out/timing01.eps" pic
    >> writeSVG_latin1 "./out/timing01.svg" pic


pic :: DPicture
pic = foldr1 picOver [pic1,pic2,pic3]

pic1 :: DPicture
pic1 = drawGraphicU $ supply zeroPt $  
         metastasis brown 2 Even 10

pic2 :: DPicture
pic2 = drawGraphicU $ supply (P2 40 0) $  
         fillRightCamferedRect grey 2 10

pic3 :: DPicture
pic3 = drawGraphicU $ supply (P2 80 0) $  
         high black FromBtm 2 10

