{-# OPTIONS -Wall #-}

-- Note - how the background is built in this example is very 
-- expensive, i.e. it generates large PostScript and SVG files
-- because the text elements are drawn many more times than they
-- are actually seen.
--
-- This example just illustrates that clipping-paths work and 
-- uses a complicated background to make that point.
--


module ClipPic where

import Wumpus.Basic.Kernel
import Wumpus.Drawing.Chains
import Wumpus.Drawing.Colour.SVGColours
import Wumpus.Drawing.Paths
import Wumpus.Drawing.Text.SafeFonts

import Wumpus.Core                              -- package: wumpus-core

import Data.AffineSpace                         -- package: vector-space

import System.Directory


main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/"
    let pic = runCtxPictureU pic_drawing_ctx big_pic
    writeEPS "./out/clip_pic.eps" pic
    writeSVG "./out/clip_pic.svg" pic


pic_drawing_ctx :: DrawingContext
pic_drawing_ctx = standardContext 14


big_pic :: DCtxPicture
big_pic = pic1 `nextToV` zconcat [cpic1, cpic2, cpic3, cpic4]

fillPath :: Num u => Path u -> Graphic u
fillPath = filledPath . toPrimPath

pic1 :: DCtxPicture
pic1 = drawTracing $
         localize (fillColour medium_slate_blue) $ do
            draw $ fillPath path01
            localize (fillColour powder_blue) $ 
                     draw $ fillPath path02
            draw $ fillPath path03
            draw $ fillPath path04


background :: RGBi -> DCtxPicture
background rgb = drawTracing $ 
    localize (strokeColour rgb) $ 
        unchain 112 iheartHaskell $ tableDown 18 (86,16) (P2 0 288)

cpic1 :: DCtxPicture 
cpic1 = clipCtxPicture (toPrimPath path01) (background black)
  
cpic2 :: DCtxPicture
cpic2 = clipCtxPicture (toPrimPath path02) (background medium_violet_red)

cpic3 :: DCtxPicture 
cpic3 = clipCtxPicture (toPrimPath path03) (background black)

cpic4 :: DCtxPicture 
cpic4 = clipCtxPicture (toPrimPath path04) (background black)


iheartHaskell :: Num u => FromPtSize u => LocGraphic u
iheartHaskell = promoteR1 $ \pt -> 
    let body  = textline "I Haskell" `at` pt
        heart = localize (fontFace symbol) $ 
                  textline "&heart;" `at` (pt .+^ hvec 7)
    in body `oplus` heart


path01 :: Floating u => Path u
path01 = execPath zeroPt $ hline 80 >> rlineto (vec 112 160) 
                                    >> rlineto (vec (-112) 160)
                                    >> hline (-80)
                                    >> rlineto (vec 112 (-160))
                                    >> rlineto (vec (-112) (-160))
 

path02 :: Floating u => Path u
path02 = execPath (P2 112 0) $ hline 80 >> rlineto (vec 72 112)
                                        >> rlineto (vec 72 (-112))
                                        >> hline 80
                                        >> rlineto (vec (-224) 320)
                                        >> hline (-80)
                                        >> rlineto (vec 112 (-160))
                                        >> rlineto (vec (-112) (-160))

path03 :: Floating u => Path u
path03 = execPath (P2 384 96) $ hline 96 >> vline 56 >> hline (-136) 

path04 :: Floating u => Path u
path04 = execPath (P2 328 192) $ hline 152 >> vline 56 >> hline (-192) 

