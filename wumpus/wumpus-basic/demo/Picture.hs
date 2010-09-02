{-# OPTIONS -Wall #-}

module Picture where

import Wumpus.Basic.Colour.SVGColours
import Wumpus.Basic.Graphic
import Wumpus.Basic.PictureLanguage
import Wumpus.Basic.SafeFonts

import Wumpus.Core                              -- package: wumpus-core

import System.Directory


main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/"
    >> writeEPS_latin1 "./out/picture_lang.eps" pic1
    >> writeSVG_latin1 "./out/picture_lang.svg" pic1 


pic1 :: DPicture
pic1 = picAnno pic "red `over` green `over` blue"
  where
    pic :: DPicture
    pic = illustrateBounds blue $ rect_red `over` rect_green `over` rect_blue


picAnno :: DPicture -> String -> DPicture
picAnno pic msg = alignH HCenter pic lbl
  where
    lbl = drawGraphicU $ supply (P2 0 0) $ 
            textline (black,FontAttr 14 courier) msg


rect_red :: DPicture
rect_red = drawGraphicU $ supply (P2 0 20) $ 
          strokedRectangle black 40 20 `cc` filledRectangle indian_red 40 20

rect_green :: DPicture
rect_green = drawGraphicU $ supply (P2 20 20) $ 
    strokedRectangle black 30 40 `cc` filledRectangle olive_drab 30 40


rect_blue :: DPicture
rect_blue = drawGraphicU $ supply (P2 20 0) $ 
    strokedRectangle black 30 30 `cc` filledRectangle powder_blue 30 30


