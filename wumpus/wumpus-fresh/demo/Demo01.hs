{-# OPTIONS -Wall #-}

module Demo01 where


import Wumpus.Fresh.FormatCombinators
import Wumpus.Fresh.FreshIR
import Wumpus.Fresh.Geometry
import Wumpus.Fresh.PostScript

demo01 :: IO ()
demo01 = printPicture pic1

pic1 :: DPicture
pic1 = frameMulti [ ellipse_ 20 10 zeroPt ]