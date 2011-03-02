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
pictures = cxpColumnSep 12 pic1 [ pic2, pic3, pic4, pic5
                                , pic6, pic7, pic8, pic9 
                                , pic10, pic11, pic12
                                ] 

drawBlueBounds :: CtxPicture -> CtxPicture
drawBlueBounds = mapCtxPicture (illustrateBounds blue)

pic1 :: CtxPicture
pic1 = picAnno pic "red `oplus` green `oplus` blue"
  where
    pic :: CtxPicture
    pic = drawBlueBounds $ rect_red `oplus` rect_green `oplus` rect_blue



pic2 :: CtxPicture
pic2 = picAnno pic "red `cxpBeneath` green `cxpBeneath` blue"
  where
    pic :: CtxPicture
    pic = drawBlueBounds $ rect_red `cxpBeneath` rect_green `cxpBeneath` rect_blue


pic3 :: CtxPicture 
pic3 = picAnno pic "red `cxpUniteCenter` green `cxpUniteCenter` blue"
  where
    pic :: CtxPicture
    pic = drawBlueBounds $ 
            rect_red `cxpUniteCenter` rect_green `cxpUniteCenter` rect_blue


-- Note - @oright@ only moves pictures in the horizontal.
--
pic4 :: CtxPicture 
pic4 = picAnno pic "red `cxpRight` green `cxpRight` blue"
  where
    pic :: CtxPicture
    pic = drawBlueBounds $ 
            rect_red `cxpRight` rect_green `cxpRight` rect_blue

-- Note - @odown@ only moves pictures in the vertical.
--
pic5 :: CtxPicture 
pic5 = picAnno pic "red `cxpDown` green `cxpDown` blue"
  where
    pic :: CtxPicture
    pic = drawBlueBounds $ 
            rect_red `cxpDown` rect_green `cxpDown` rect_blue


pic6 :: CtxPicture
pic6 = picAnno pic "oconcat red [green, blue]"
  where
    pic :: CtxPicture
    pic = drawBlueBounds $ 
            oconcat rect_red [rect_green, rect_blue]


pic7 :: CtxPicture
pic7 = picAnno pic "cxpRow red [green, blue]"
  where
    pic :: CtxPicture
    pic = drawBlueBounds $ 
            cxpRow rect_red [rect_green, rect_blue]


pic8 :: CtxPicture
pic8 = picAnno pic "cxpColumn red [green, blue]"
  where
    pic :: CtxPicture
    pic = drawBlueBounds $ 
            cxpColumn rect_red [rect_green, rect_blue]


-- Note - API naming, where functions are called Sep they should 
-- be changed to Space (@sep@ implies a separator, no necessarily
-- a space in pretty-print terms).
--

pic9 :: CtxPicture
pic9 = picAnno pic "cxpRowSep 10 red [green, blue]"
  where
    pic :: CtxPicture
    pic = drawBlueBounds $ 
            cxpRowSep 10 rect_red [rect_green, rect_blue]

pic10 :: CtxPicture
pic10 = picAnno pic "cxpColumnSep 10 red [green, blue]"
  where
    pic :: CtxPicture
    pic = drawBlueBounds $ 
            cxpColumnSep 10 rect_red [rect_green, rect_blue]


pic11 :: CtxPicture
pic11 = picAnno pic "cxpAlignRow HTop red [green, blue]"
  where
    pic :: CtxPicture
    pic = drawBlueBounds $ 
            cxpAlignRow HTop rect_red [rect_green, rect_blue]


pic12 :: CtxPicture
pic12 = picAnno pic "cxpAlignColumn VRight red [green, blue]"
  where
    pic :: CtxPicture
    pic = drawBlueBounds $ 
            cxpAlignColumn VRight rect_red [rect_green, rect_blue]


--------------------------------------------------------------------------------


picAnno :: CtxPicture -> String -> CtxPicture
picAnno pic msg = cxpAlignSepH HCenter 30 pic lbl
  where
    lbl = drawTracing $ draw $ textline msg `at` zeroPt


rect_red :: CtxPicture
rect_red = drawTracing $ 
    localize (fill_colour indian_red)
             (draw $ borderedRectangle 30 10 `at` (P2 0 10))
                 
rect_green :: CtxPicture
rect_green = drawTracing $ 
    localize (fill_colour olive_drab)
             (draw $ borderedRectangle 15 15 `at` (P2 10 10))


rect_blue :: CtxPicture
rect_blue = drawTracing $ 
    localize (fill_colour powder_blue)
             (draw $ borderedRectangle 20 30 `at` (P2 10 0))



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
