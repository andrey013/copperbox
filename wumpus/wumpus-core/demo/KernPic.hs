{-# OPTIONS -Wall #-}

module KernPic where

import Wumpus.Core
import Wumpus.Core.Colour

import System.Directory



main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/"
    writeEPS_latin1 "./out/kern_pic01.eps" kern_pic
    writeSVG_latin1 "./out/kern_pic01.svg" kern_pic


kern_pic :: DPicture
kern_pic = pic1 `picOver` pic2

pic1 :: DPicture
pic1 = frame [ helveticaLabelH mystere   (P2 0 40)
             , helveticaLabelH mystere   (P2 0 20)
             ]

pic2 :: DPicture
pic2 = illustrateBoundsPrim blue_violet $ 
          helveticaLabelV mystere   (P2 100 140)


mystere ::[DKerningChar]
mystere = [ kernchar 0  'm'
          , kernchar 15 'y'
          , kernchar 10 's'
          , kernchar 10 't'
          , kernEscInt 6 232
          , kernchar 10 'r'
          , kernchar 6 'e'
          ]

helveticaLabelH :: [KerningChar Double] -> DPoint2 -> DPrimitive
helveticaLabelH xs pt = hkernlabel black helvetica18 xs pt

helveticaLabelV :: [KerningChar Double] -> DPoint2 -> DPrimitive
helveticaLabelV xs pt = vkernlabel black helvetica18 xs pt


helvetica18 :: FontAttr
helvetica18 = FontAttr 18 (FontFace "Helvetica" "Helvetica" SVG_REGULAR)

blue_violet             :: RGBi
blue_violet             = RGBi 0x8a 0x2b 0xe2