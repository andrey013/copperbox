{-# OPTIONS -Wall #-}

module Picture where

import Wumpus.Basic.Colour.SVGColours
import Wumpus.Basic.DrawingComposition
import Wumpus.Basic.Graphic

import Wumpus.Core                              -- package: wumpus-core

import System.Directory


main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/"
    let out1 = runDrawingU pic_drawing_ctx pictures
    writeEPS "./out/picture_lang.eps" out1
    writeSVG "./out/picture_lang.svg" out1


pic_drawing_ctx :: DrawingContext
pic_drawing_ctx = standardContext 14


pictures :: DDrawing
pictures = vsep 12 [ pic1,  pic2,  pic3,  pic4
                   , pic5,  pic6,  pic7,  pic8
                   , pic9,  pic10, pic11, pic12 ]


drawBlueBounds :: (Real u, Floating u, FromPtSize u) 
               => Drawing u -> Drawing u
drawBlueBounds = modifyDrawing (illustrateBounds blue)

pic1 :: DDrawing
pic1 = picAnno pic "red `over` green `over` blue"
  where
    pic :: DDrawing
    pic = drawBlueBounds $ rect_red `over` rect_green `over` rect_blue

pic2 :: DDrawing
pic2 = picAnno pic "red `under` green `under` blue"
  where
    pic :: DDrawing
    pic = drawBlueBounds $ rect_red `under` rect_green `under` rect_blue


pic3 :: DDrawing 
pic3 = picAnno pic "red `centerOver` green `centerOver` blue"
  where
    pic :: DDrawing
    pic = drawBlueBounds $ 
            rect_red `centerOver` rect_green `centerOver` rect_blue

-- Note - nextToH only moves pictures in the horizontal.
--
pic4 :: DDrawing 
pic4 = picAnno pic "red `nextToH` green `nextToH` blue"
  where
    pic :: DDrawing
    pic = drawBlueBounds $ 
            rect_red `nextToH` rect_green `nextToH` rect_blue

-- Note - nextToV only moves pictures in the vertical.
--
pic5 :: DDrawing 
pic5 = picAnno pic "red `nextToV` green `nextToV` blue"
  where
    pic :: DDrawing
    pic = drawBlueBounds $ 
            rect_red `nextToV` rect_green `nextToV` rect_blue


pic6 :: DDrawing
pic6 = picAnno pic "zconcat [red, green, blue]"
  where
    pic :: DDrawing
    pic = drawBlueBounds $ 
            zconcat [rect_red, rect_green, rect_blue]


pic7 :: DDrawing
pic7 = picAnno pic "hcat [red, green, blue]"
  where
    pic :: DDrawing
    pic = drawBlueBounds $ 
            hcat [rect_red, rect_green, rect_blue]

pic8 :: DDrawing
pic8 = picAnno pic "vcat [red, green, blue]"
  where
    pic :: DDrawing
    pic = drawBlueBounds $ 
            vcat [rect_red, rect_green, rect_blue]

pic9 :: DDrawing
pic9 = picAnno pic "hsep 20 [red, green, blue]"
  where
    pic :: DDrawing
    pic = drawBlueBounds $ 
            hsep 20 [rect_red, rect_green, rect_blue]

pic10 :: DDrawing
pic10 = picAnno pic "vsep 20 [red, green, blue]"
  where
    pic :: DDrawing
    pic = drawBlueBounds $ 
            vsep 20 [rect_red, rect_green, rect_blue]


pic11 :: DDrawing
pic11 = picAnno pic "hcatA HTop [red, green, blue]"
  where
    pic :: DDrawing
    pic = drawBlueBounds $ 
            hcatA HTop [rect_red, rect_green, rect_blue]


pic12 :: DDrawing
pic12 = picAnno pic "vcatA VCenter [red, green, blue]"
  where
    pic :: DDrawing
    pic = drawBlueBounds $ 
            vcatA VCenter [rect_red, rect_green, rect_blue]


--------------------------------------------------------------------------------


picAnno :: DDrawing -> String -> DDrawing
picAnno pic msg = alignHSep HCenter 30 pic lbl
  where
    lbl = drawTracing $ draw $ textline msg `at` zeroPt


rect_red :: DDrawing
rect_red = drawTracing $ 
    localize (fillColour indian_red)
             (draw $ borderedRectangle 30 10 `at` (P2 0 10))
                 
rect_green :: DDrawing
rect_green = drawTracing $ 
    localize (fillColour olive_drab)
             (draw $ borderedRectangle 15 15 `at` (P2 10 10))


rect_blue :: DDrawing
rect_blue = drawTracing $ 
    localize (fillColour powder_blue)
             (draw $ borderedRectangle 20 30 `at` (P2 10 0))

