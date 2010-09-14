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
pictures = vsep 12 pic1 [ pic2,  pic3,  pic4,  pic5
                        , pic6,  pic7,  pic8,  pic9
                        , pic10, pic11, pic12, pic13 ]

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
pic3 = picAnno pic "red `centerOver` green `centerOver` blue"
  where
    pic :: DPicture
    pic = illustrateBounds blue $ 
            rect_red `centerOver` rect_green `centerOver` rect_blue

-- Note - nextToH only moves pictures in the horizontal.
--
pic4 :: DPicture 
pic4 = picAnno pic "red `nextToH` green `nextToH` blue"
  where
    pic :: DPicture
    pic = illustrateBounds blue $ 
            rect_red `nextToH` rect_green `nextToH` rect_blue

-- Note - nextToV only moves pictures in the vertical.
--
pic5 :: DPicture 
pic5 = picAnno pic "red `nextToV` green `nextToV` blue"
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
pic8 = picAnno pic "hcat red [green, blue]"
  where
    pic :: DPicture
    pic = illustrateBounds blue $ 
            hcat rect_red [rect_green, rect_blue]

pic9 :: DPicture
pic9 = picAnno pic "vcat red [green, blue]"
  where
    pic :: DPicture
    pic = illustrateBounds blue $ 
            vcat rect_red [rect_green, rect_blue]

pic10 :: DPicture
pic10 = picAnno pic "hsep 20 red [green, blue]"
  where
    pic :: DPicture
    pic = illustrateBounds blue $ 
            hsep 20 rect_red [rect_green, rect_blue]

pic11 :: DPicture
pic11 = picAnno pic "vsep 20 red [green, blue]"
  where
    pic :: DPicture
    pic = illustrateBounds blue $ 
            vsep 20 rect_red [rect_green, rect_blue]


pic12 :: DPicture
pic12 = picAnno pic "hcatA HTop red [green, blue]"
  where
    pic :: DPicture
    pic = illustrateBounds blue $ 
            hcatA HTop rect_red [rect_green, rect_blue]


pic13 :: DPicture
pic13 = picAnno pic "vcatA VCenter red [green, blue]"
  where
    pic :: DPicture
    pic = illustrateBounds blue $ 
            vcatA VCenter rect_red [rect_green, rect_blue]


--------------------------------------------------------------------------------

pic_drawing_ctx :: DrawingContext
pic_drawing_ctx = standardContext 14

picAnno :: DPicture -> String -> DPicture
picAnno pic msg = alignHSep HCenter 30 pic lbl
  where
    lbl = liftToPictureU $ execDrawing pic_drawing_ctx $ 
            drawAt zeroPt (textline msg)


rect_red :: DPicture
rect_red = liftToPictureU $ execDrawing pic_drawing_ctx $ 
              localCtx (secondaryColour indian_red)
                       (drawAt (P2 0 10) $ borderedRectangle 30 10)
                 
rect_green :: DPicture
rect_green = liftToPictureU $ execDrawing pic_drawing_ctx $ 
              localCtx (secondaryColour olive_drab)
                       (drawAt (P2 10 10) $ borderedRectangle 15 15)


rect_blue :: DPicture
rect_blue = liftToPictureU $ execDrawing pic_drawing_ctx $ 
              localCtx (secondaryColour powder_blue)
                       (drawAt (P2 10 0) $ borderedRectangle 20 30)

