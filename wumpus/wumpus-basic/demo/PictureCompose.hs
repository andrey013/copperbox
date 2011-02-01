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


pictures :: DCtxPicture
pictures = vsepPic 12 [ pic1, pic2, pic4, pic5 ] 

{-
pictures = vsep 12 [ pic1,  pic2,  pic3,  pic4
                   , pic5,  pic6,  pic7,  pic8
                   , pic9,  pic10, pic11, pic12 ]
-}

drawBlueBounds :: (Real u, Floating u, FromPtSize u) 
               => CtxPicture u -> CtxPicture u
drawBlueBounds = mapCtxPicture (illustrateBounds blue)

pic1 :: DCtxPicture
pic1 = picAnno pic "red `oplus` green `oplus` blue"
  where
    pic :: DCtxPicture
    pic = drawBlueBounds $ rect_red `oplus` rect_green `oplus` rect_blue



pic2 :: DCtxPicture
pic2 = picAnno pic "red `beneath` green `beneath` blue"
  where
    pic :: DCtxPicture
    pic = drawBlueBounds $ rect_red `beneath` rect_green `beneath` rect_blue

{-
pic3 :: DCtxPicture 
pic3 = picAnno pic "red `centric` green `centric` blue"
  where
    pic :: DCtxPicture
    pic = drawBlueBounds $ 
            rect_red `centric` rect_green `centric` rect_blue
-}

-- Note - @oright@ only moves pictures in the horizontal.
--
pic4 :: DCtxPicture 
pic4 = picAnno pic "red `oright` green `oright` blue"
  where
    pic :: DCtxPicture
    pic = drawBlueBounds $ 
            rect_red `oright` rect_green `oright` rect_blue

-- Note - @odown@ only moves pictures in the vertical.
--
pic5 :: DCtxPicture 
pic5 = picAnno pic "red `odown` green `odown` blue"
  where
    pic :: DCtxPicture
    pic = drawBlueBounds $ 
            rect_red `odown` rect_green `odown` rect_blue

{-
pic6 :: DCtxPicture
pic6 = picAnno pic "zconcat [red, green, blue]"
  where
    pic :: DCtxPicture
    pic = drawBlueBounds $ 
            zconcat [rect_red, rect_green, rect_blue]


pic7 :: DCtxPicture
pic7 = picAnno pic "hcat [red, green, blue]"
  where
    pic :: DCtxPicture
    pic = drawBlueBounds $ 
            hcat [rect_red, rect_green, rect_blue]

pic8 :: DCtxPicture
pic8 = picAnno pic "vcat [red, green, blue]"
  where
    pic :: DCtxPicture
    pic = drawBlueBounds $ 
            vcat [rect_red, rect_green, rect_blue]

pic9 :: DCtxPicture
pic9 = picAnno pic "hsep 20 [red, green, blue]"
  where
    pic :: DCtxPicture
    pic = drawBlueBounds $ 
            hsep 20 [rect_red, rect_green, rect_blue]

pic10 :: DCtxPicture
pic10 = picAnno pic "vsep 20 [red, green, blue]"
  where
    pic :: DCtxPicture
    pic = drawBlueBounds $ 
            vsep 20 [rect_red, rect_green, rect_blue]


pic11 :: DCtxPicture
pic11 = picAnno pic "hcatA HTop [red, green, blue]"
  where
    pic :: DCtxPicture
    pic = drawBlueBounds $ 
            hcatA HTop [rect_red, rect_green, rect_blue]


pic12 :: DCtxPicture
pic12 = picAnno pic "vcatA VCenter [red, green, blue]"
  where
    pic :: DCtxPicture
    pic = drawBlueBounds $ 
            vcatA VCenter [rect_red, rect_green, rect_blue]

-}
--------------------------------------------------------------------------------


picAnno :: DCtxPicture -> String -> DCtxPicture
picAnno pic msg = alignHSep HCenter 30 pic lbl
  where
    lbl = drawTracing $ draw $ textline msg `at` zeroPt


rect_red :: DCtxPicture
rect_red = drawTracing $ 
    localize (fillColour indian_red)
             (draw $ borderedRectangle 30 10 `at` (P2 0 10))
                 
rect_green :: DCtxPicture
rect_green = drawTracing $ 
    localize (fillColour olive_drab)
             (draw $ borderedRectangle 15 15 `at` (P2 10 10))


rect_blue :: DCtxPicture
rect_blue = drawTracing $ 
    localize (fillColour powder_blue)
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
