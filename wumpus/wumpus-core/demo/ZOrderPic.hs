{-# OPTIONS -Wall #-}

module ZOrderPic where

import Wumpus.Core
import Wumpus.Core.Colour

import Data.AffineSpace                 -- package: vector-space

import System.Directory

main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/"
    writeEPS_latin1 "./out/zorder01.eps" combined_pic
    writeSVG_latin1 "./out/zorder01.svg" combined_pic


combined_pic :: DPicture
combined_pic = multi [pic1,pic2]

pic1 :: DPicture
pic1 = frameMulti $ prim_list zeroPt

pic2 :: DPicture 
pic2 = multi $ map frame $ prim_list (P2 200 0)



prim_list :: DPoint2 -> [DPrimitive]
prim_list = sequence [ ellipse red   20 20
                     , \p -> ellipse green 20 20 (p .+^ hvec 20)
                     , \p -> ellipse blue  20 20 (p .+^ hvec 40)
                     ]
