{-# OPTIONS -Wall #-}

module PictureCompose where

import Wumpus.Basic.Kernel

import Wumpus.Core                              -- package: wumpus-core

import System.Directory


main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/"
    let out1 = runCtxPictureU pic_drawing_ctx pictures
    writeEPS "./out/picture_composition.eps" out1
    writeSVG "./out/picture_composition.svg" out1


pic_drawing_ctx :: DrawingContext
pic_drawing_ctx = standardContext 14


pictures :: CtxPicture
pictures = vsep (12::Double) [ pic1, pic2, pic3, pic4
                             , pic5, pic6, pic7, pic8
                             , pic9, pic10, pic11, pic12
                             ] 

drawBlueBounds :: CtxPicture -> CtxPicture
drawBlueBounds = mapCtxPicture (illustrateBounds blue)

pic1 :: CtxPicture
pic1 = picAnno pic "red `superior` green `superior` blue"
  where
    pic :: CtxPicture
    pic = drawBlueBounds $ rect_red `superior` rect_green `superior` rect_blue



pic2 :: CtxPicture
pic2 = picAnno pic "red `anterior` green `anterior` blue"
  where
    pic :: CtxPicture
    pic = drawBlueBounds $ rect_red `anterior` rect_green `anterior` rect_blue


pic3 :: CtxPicture 
pic3 = picAnno pic "red `uniteCenter` green `uniteCenter` blue"
  where
    pic :: CtxPicture
    pic = drawBlueBounds $ 
            rect_red `uniteCenter` rect_green `uniteCenter` rect_blue


-- Note - @oright@ only moves pictures in the horizontal.
--
pic4 :: CtxPicture 
pic4 = picAnno pic "red `hconcat` green `hconcat` blue"
  where
    pic :: CtxPicture
    pic = drawBlueBounds $ 
            rect_red `hconcat` rect_green `hconcat` rect_blue

-- Note - @odown@ only moves pictures in the vertical.
--
pic5 :: CtxPicture 
pic5 = picAnno pic "red `vconcat` green `vconcat` blue"
  where
    pic :: CtxPicture
    pic = drawBlueBounds $ 
            rect_red `vconcat` rect_green `vconcat` rect_blue


pic6 :: CtxPicture
pic6 = picAnno pic "cat [red, green, blue]"
  where
    pic :: CtxPicture
    pic = drawBlueBounds $ 
            cat [rect_red, rect_green, rect_blue]


pic7 :: CtxPicture
pic7 = picAnno pic "hcat [red, green, blue]"
  where
    pic :: CtxPicture
    pic = drawBlueBounds $ 
            hcat [rect_red, rect_green, rect_blue]


pic8 :: CtxPicture
pic8 = picAnno pic "vcat [red,green, blue]"
  where
    pic :: CtxPicture
    pic = drawBlueBounds $ 
            vcat [rect_red, rect_green, rect_blue]


-- Note - API naming, where functions are called Sep they should 
-- be changed to Space (@sep@ implies a separator, no necessarily
-- a space in pretty-print terms).
--

pic9 :: CtxPicture
pic9 = picAnno pic "hsep 10 [red, green, blue]"
  where
    pic :: CtxPicture
    pic = drawBlueBounds $ 
            hsep (10::Double) [rect_red, rect_green, rect_blue]

pic10 :: CtxPicture
pic10 = picAnno pic "vsep 10 [red, green, blue]"
  where
    pic :: CtxPicture
    pic = drawBlueBounds $ 
            vsep (10::Double) [rect_red, rect_green, rect_blue]


pic11 :: CtxPicture
pic11 = picAnno pic "alignRow HTop [red, green, blue]"
  where
    pic :: CtxPicture
    pic = drawBlueBounds $ 
            alignRow HTop [rect_red, rect_green, rect_blue]


pic12 :: CtxPicture
pic12 = picAnno pic "alignColumn VRight [red, green, blue]"
  where
    pic :: CtxPicture
    pic = drawBlueBounds $ 
            alignColumn VRight [rect_red, rect_green, rect_blue]


--------------------------------------------------------------------------------



picAnno :: CtxPicture -> String -> CtxPicture
picAnno pic msg = halignSpace HCenter (30::Double) pic lbl
  where
    lbl = drawTracing $ body
    body :: TraceDrawing Double ()
    body = draw $ dcTextlabel msg `at` zeroPt 


rect_red :: CtxPicture
rect_red = drawTracing $ body
  where  
    body :: TraceDrawing Double ()
    body = localize (fill_colour indian_red)
                    (draw $ dcRectangle FILL_STROKE 30 10 `at` (P2 0 10))
                 
rect_green :: CtxPicture
rect_green = drawTracing $ body
  where
    body :: TraceDrawing Double ()
    body = localize (fill_colour olive_drab)
                    (draw $ dcRectangle FILL_STROKE 15 15 `at` (P2 10 10))


rect_blue :: CtxPicture
rect_blue = drawTracing $ body
  where
    body :: TraceDrawing Double ()
    body = localize (fill_colour powder_blue)
                    (draw $ dcRectangle FILL_STROKE 20 30 `at` (P2 10 0))



-- Copied from Wumpus-Drawing (we don\'t a circular dependency).
--

blue                    :: RGBi
blue                    = RGBi 0x00 0x00 0xff

indian_red              :: RGBi
indian_red              = RGBi 0xcd 0x5c 0x5c

olive_drab              :: RGBi
olive_drab              = RGBi 0x6b 0x8e 0x23

powder_blue             :: RGBi
powder_blue             = RGBi 0xb0 0xe0 0xe6
