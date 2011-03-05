{-# OPTIONS -Wall #-}

module NullPic where

import Wumpus.Core
import Wumpus.Core.Colour

import System.Directory

main :: IO ()
main = do 
    createDirectoryIfMissing True "./out"
    writeEPS "./out/null_pic.eps" pic_empty
    writeSVG "./out/null_pic.svg" pic_empty



pic_empty :: Picture
pic_empty = 
    frame [ ostroke red  default_stroke_attr null_path
          , ostroke blue default_stroke_attr (vectorPrimPath (P2 0 100) [])
          , ztextlabel "" zeroPt 
          ]
  where
   null_path = emptyPrimPath $ P2 100 100



