{-# OPTIONS -Wall #-}

module MultiPic where

import Wumpus.Core
import Wumpus.Core.Colour

import Data.AffineSpace

import System.Directory

main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/"
    writeEPS_latin1 "./out/multi_pic.eps"  pic1
    writeSVG_latin1 "./out/multi_pic.svg"  pic1



pic1 :: DPicture
pic1 = uniformScale 2 $ frame $ 
    [ ellipse blue 10 10 zeroPt
    , ellipse red 10 10 (P2 40 40)
    , ztextlabel "Wumpus!" (P2 40 20)
    , square red 5 (P2 50 10)  
    ]


square :: (Num u, Ord u) => RGBi -> u -> Point2 u -> Primitive u
square rgb sidelen bl = fill rgb $ vertexPath $
    [bl, bl .+^ hvec sidelen, bl .+^ V2 sidelen sidelen, bl .+^ vvec sidelen]

-- The PostScript generated from this is pretty good.
-- 
-- No extraneous use of @concat@.
--
