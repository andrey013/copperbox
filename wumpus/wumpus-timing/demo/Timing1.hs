{-# OPTIONS -Wall #-}

module Timing1 where

import Wumpus.Timing.Drawing hiding ( glitch )
import Wumpus.Timing.Interpret
import Wumpus.Timing.TimingMonad


import Wumpus.Core                      -- package: wumpus-core
import Wumpus.Basic.Graphic             -- package: wumpus-basic
import Wumpus.Basic.SVGColours

import System.Directory


main :: IO ()
main = createDirectoryIfMissing True "./out/"
    >> writeEPS_latin1 "./out/timing01.eps" pic0
    >> writeSVG_latin1 "./out/timing01.svg" pic0


pic0 :: DPicture
pic0 = drawGraphicU $ interpret $ evalTimingM $ do 
         { high; high; highImp; undef; low; high; glitch; high ; high }

pic :: DPicture
pic = foldr1 picOver [pic1,pic2,pic3]

pic1 :: DPicture
pic1 = drawGraphicU $ supply zeroPt $  
         metastasis FromBtm 2 Even 10 (defaultProps { stroke_colour = brown })

pic2 :: DPicture
pic2 = drawGraphicU $ supply (P2 40 0) $  
         fillRightCamferedRect grey 2 10

pic3 :: DPicture
pic3 = drawGraphicU $ supply (P2 80 0) $  
         lineHigh FromBtm 2 10 defaultProps


