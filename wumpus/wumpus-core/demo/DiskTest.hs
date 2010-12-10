{-# OPTIONS -Wall #-}

-- Check SVG output - circle should not have extra matrix trafo,
-- whereas text needs the trafo.

module Disktest where

import Wumpus.Core
import System.Directory




main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/"
    writeEPS "./out/disk_test.eps" pic1
    writeSVG "./out/disk_test.svg" pic1


pic1 :: DPicture
pic1 = frame  
          [ ztextlabel "Disk"  (P2 0 0)
          , zellipse 5 5 (P2 50 5)
          , ztextlabel "should not have a matrix trafo in the SVG." (P2 68 0)
          ]
