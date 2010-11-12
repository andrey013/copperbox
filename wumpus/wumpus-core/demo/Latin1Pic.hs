{-# OPTIONS -Wall #-}

module Latin1Pic where

import Wumpus.Core
import Wumpus.Core.Colour
import Wumpus.Core.Text.StandardEncoding

import System.Directory



main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/"
    writeEPS "./out/latin1_pic01.eps" pic1
    writeSVG "./out/latin1_pic01.svg" pic1


-- | Provided the respective lookups can be found, Wumpus 
-- supports escapes as either numbers or names...
--
pic1 :: DPicture
pic1 = frame [ helveticaLabel "myst&#232;re"      (P2 0 60)
             , helveticaLabel "myst&#egrave;re"   (P2 0 40)
             , helveticaLabel "myst&#0o350;re"    (P2 0 20)
             , helveticaLabel "myst&#0XE8;re"     (P2 0 00)
             ]
 



helveticaLabel :: String -> DPoint2 -> DPrimitive
helveticaLabel ss pt = textlabel black helvetica18 ss pt

helvetica18 :: FontAttr
helvetica18 = FontAttr 18 (FontFace "Helvetica" "Helvetica" 
                                    SVG_REGULAR standard_encoding)
