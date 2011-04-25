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

import Wumpus.Drawing.Colour.SVGColours
import Wumpus.Drawing.Paths.Absolute
import Wumpus.Drawing.Text.StandardFontDefs

import Wumpus.Basic.Kernel                      -- package: wumpus-basic

import Wumpus.Core                              -- package: wumpus-core

import Data.AffineSpace                         -- package: vector-space

import System.Directory


main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/"
    let pic1 = runCtxPictureU std_ctx $ top_pic `vconcat` clip_pic
    writeEPS "./out/clip_pic.eps" pic1
    writeSVG "./out/clip_pic.svg" pic1


std_ctx :: DrawingContext
std_ctx = standardContext 14


top_pic :: CtxPicture
top_pic = drawTracing $ localize (fill_colour medium_slate_blue) $ do
    draw $ toPrimPath path01 >>= dcClosedPath FILL
    draw $ localize (fill_colour powder_blue) $ toPrimPath path02 >>= dcClosedPath FILL
    draw $ toPrimPath path03 >>= dcClosedPath FILL
    draw $ toPrimPath path04 >>= dcClosedPath FILL

clip_pic :: CtxPicture
clip_pic = drawTracing $ do
    mapM_ draw $ [ clip1, clip2, clip3, clip4 ]


background :: RGBi -> Graphic Double
background rgb = 
    fmap ignoreAns $ localize (text_colour rgb) $ ihh `at` P2 0 288
  where
    ihh = chain (tableDown 18 (86,16)) (replicate 112 iheartHaskell)

-- Wumpus-Basic needs a clip function, but is this the most 
-- satisfactory definition?
--
clipGraphic :: PrimPath -> Graphic u -> Graphic u 
clipGraphic cp = fmap (clipObject cp)


clip1 :: Graphic Double
clip1 = toPrimPath path01 >>= \pp -> clipGraphic pp (background black)
  
clip2 :: Graphic Double
clip2 = toPrimPath path02 >>= \pp -> clipGraphic pp (background medium_violet_red)

clip3 :: Graphic Double
clip3 = toPrimPath path03 >>= \pp -> clipGraphic pp (background black)

clip4 :: Graphic Double
clip4 = toPrimPath path04 >>= \pp -> clipGraphic pp (background black)


iheartHaskell :: LocGraphic Double
iheartHaskell = promoteR1 $ \pt -> 
    let body  = dcTextlabel "I Haskell" `at` pt
        heart = localize (set_font symbol) $ 
                  dcTextlabel "&heart;" `at` (pt .+^ hvec 7)
    in body `oplus` heart


path01 :: AbsPath Double
path01 = evalAbsBuild zeroPt $  hline 80 
                             >> relline (vec 112 160) 
                             >> relline (vec (-112) 160)
                             >> hline (-80)
                             >> relline (vec 112 (-160))
                             >> relline (vec (-112) (-160))
 

path02 :: AbsPath Double
path02 = evalAbsBuild (P2 112 0) $  hline 80 
                                 >> relline (vec 72 112)
                                 >> relline (vec 72 (-112))
                                 >> hline 80
                                 >> relline (vec (-224) 320)
                                 >> hline (-80)
                                 >> relline (vec 112 (-160))
                                 >> relline (vec (-112) (-160))

path03 :: AbsPath Double
path03 = evalAbsBuild (P2 384 96) $ hline 96 >> vline 56 >> hline (-136) 

path04 :: AbsPath Double
path04 = evalAbsBuild (P2 328 192) $ hline 152 >> vline 56 >> hline (-192) 

