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
    >> writeEPS_latin1 "./out/picture_lang.eps" pictures
    >> writeSVG_latin1 "./out/picture_lang.svg" pictures

pictures :: DPicture
pictures = vsep 20 pic12 [ pic13, pic14 ]

pic1 :: DPicture
pic1 = picAnno pic "red `over` green `over` blue"
  where
    pic :: DPicture
    pic = illustrateBounds blue $ rect_red `over` rect_green `over` rect_blue

pic2 :: DPicture
pic2 = picAnno pic "red `under` green `under` blue"
  where
    pic :: DPicture
    pic = illustrateBounds blue $ rect_red `under` rect_green `under` rect_blue


pic3 :: DPicture 
pic3 = picAnno pic "rect_red `centerOver` rect_green `centerOver` rect_blue"
  where
    pic :: DPicture
    pic = illustrateBounds blue $ 
            rect_red `centerOver` rect_green `centerOver` rect_blue

-- Note - nextToH only moves pictures in the horizontal.
--
pic4 :: DPicture 
pic4 = picAnno pic "rect_red `nextToH` rect_green `nextToH` rect_blue"
  where
    pic :: DPicture
    pic = illustrateBounds blue $ 
            rect_red `nextToH` rect_green `nextToH` rect_blue

-- Note - nextToV only moves pictures in the vertical.
--
pic5 :: DPicture 
pic5 = picAnno pic "rect_red `nextToV` rect_green `nextToV` rect_blue"
  where
    pic :: DPicture
    pic = illustrateBounds blue $ 
            rect_red `nextToV` rect_green `nextToV` rect_blue


pic6 :: DPicture
pic6 = picAnno pic "[red, green] `stackOver` blue"
  where
    pic :: DPicture
    pic = illustrateBounds blue $ 
            [rect_red, rect_green] `stackOver` rect_blue



pic7 :: DPicture
pic7 = picAnno pic "zconcat red [green, blue]"
  where
    pic :: DPicture
    pic = illustrateBounds blue $ 
            zconcat rect_red [rect_green, rect_blue]


pic8 :: DPicture
pic8 = picAnno pic "zconcat red [green, blue]"
  where
    pic :: DPicture
    pic = illustrateBounds blue $ 
            zconcat rect_red [rect_green, rect_blue]


pic9 :: DPicture
pic9 = picAnno pic "hcat red [green, blue]"
  where
    pic :: DPicture
    pic = illustrateBounds blue $ 
            hcat rect_red [rect_green, rect_blue]

pic10 :: DPicture
pic10 = picAnno pic "vcat red [green, blue]"
  where
    pic :: DPicture
    pic = illustrateBounds blue $ 
            vcat rect_red [rect_green, rect_blue]

pic11 :: DPicture
pic11 = picAnno pic "hsep 20 red [green, blue]"
  where
    pic :: DPicture
    pic = illustrateBounds blue $ 
            hsep 20 rect_red [rect_green, rect_blue]

pic12 :: DPicture
pic12 = picAnno pic "vsep 20 red [green, blue]"
  where
    pic :: DPicture
    pic = illustrateBounds blue $ 
            vsep 20 rect_red [rect_green, rect_blue]


pic13 :: DPicture
pic13 = picAnno pic "hcatA HTop red [green, blue]"
  where
    pic :: DPicture
    pic = illustrateBounds blue $ 
            hcatA HTop rect_red [rect_green, rect_blue]


pic14 :: DPicture
pic14 = picAnno pic "vcatA VCenter red [green, blue]"
  where
    pic :: DPicture
    pic = illustrateBounds blue $ 
            vcatA VCenter rect_red [rect_green, rect_blue]


--------------------------------------------------------------------------------

picAnno :: DPicture -> String -> DPicture
picAnno pic msg = alignHSep HCenter 30 pic lbl
  where
    lbl = drawGraphicU $ supply (P2 0 0) $ 
            textline (black,FontAttr 14 courier) msg


rect_red :: DPicture
rect_red = drawGraphicU $ supply (P2 0 20) $ 
          strokedRectangle black 40 20 `cc` filledRectangle indian_red 40 20

rect_green :: DPicture
rect_green = drawGraphicU $ supply (P2 20 20) $ 
    strokedRectangle black 25 25 `cc` filledRectangle olive_drab 25 25


rect_blue :: DPicture
rect_blue = drawGraphicU $ supply (P2 20 0) $ 
    strokedRectangle black 30 40 `cc` filledRectangle powder_blue 30 40


