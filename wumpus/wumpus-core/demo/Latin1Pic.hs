{-# OPTIONS -Wall #-}

module Latin1Pic where

import Wumpus.Core
import Wumpus.Core.Colour

import System.Directory



main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/"
    writeEPS_latin1 "./out/latin1_pic01.eps" pic1
    writeSVG_latin1 "./out/latin1_pic01.svg" pic1


-- | Provided the respective lookups can be found, Wumpus 
-- supports escapes as either numbers or names...
--
pic1 :: DPicture
pic1 = frame [ helveticaLabel "myst&#232;re"      (P2 0 40)
             , helveticaLabel "myst&#egrave;re"   (P2 0 20)
             ]
 



helveticaLabel :: String -> DPoint2 -> DPrimitive
helveticaLabel ss pt = textlabel black helvetica18 ss pt

helvetica18 :: FontAttr
helvetica18 = FontAttr 18 (FontFace "Helvetica" "Helvetica" SVG_REGULAR)
