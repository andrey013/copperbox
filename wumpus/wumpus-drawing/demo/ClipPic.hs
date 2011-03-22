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

import Wumpus.Drawing.Chains
import Wumpus.Drawing.Colour.SVGColours
import Wumpus.Drawing.Paths
import Wumpus.Drawing.Paths.MonadicConstruction
import Wumpus.Drawing.Text.StandardFontDefs

import Wumpus.Basic.Kernel                      -- package: wumpus-basic

import Wumpus.Core                              -- package: wumpus-core

import Data.AffineSpace                         -- package: vector-space

import System.Directory


main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/"
    let pic1 = runCtxPictureU std_ctx $ top_pic `cxpDown` clip_pic
    writeEPS "./out/clip_pic.eps" pic1
    writeSVG "./out/clip_pic.svg" pic1


std_ctx :: DrawingContext
std_ctx = standardContext 14


top_pic :: CtxPicture
top_pic = drawTracing $ localize (fill_colour medium_slate_blue) $ do
    draw $ toPrimPath path01 >>= filledPath
    draw $ localize (fill_colour powder_blue) $ toPrimPath path02 >>= filledPath
    draw $ toPrimPath path03 >>= filledPath
    draw $ toPrimPath path04 >>= filledPath

clip_pic :: CtxPicture
clip_pic = drawTracing $ do
    mapM_ draw $ [ clip1, clip2, clip3, clip4 ]


background :: RGBi -> Graphic Double
background rgb = 
    ignoreAns $ localize (text_colour rgb) $ ihh `at` P2 0 288
  where
    ihh = tableDown 18 (86,16) (replicate 112 iheartHaskell)

-- Wumpus-Basic needs a clip function, but is this the most 
-- satisfactory definition?
--
clipGraphic :: PrimPath -> Graphic u -> Graphic u 
clipGraphic cp = clipObject cp


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
    let body  = textline "I Haskell" `at` pt
        heart = localize (set_font symbol) $ 
                  textline "&heart;" `at` (pt .+^ hvec 7)
    in body `oplus` heart


path01 :: Path Double
path01 = execPath zeroPt $ hline 80 >> rlineto (vec 112 160) 
                                    >> rlineto (vec (-112) 160)
                                    >> hline (-80)
                                    >> rlineto (vec 112 (-160))
                                    >> rlineto (vec (-112) (-160))
 

path02 :: Path Double
path02 = execPath (P2 112 0) $ hline 80 >> rlineto (vec 72 112)
                                        >> rlineto (vec 72 (-112))
                                        >> hline 80
                                        >> rlineto (vec (-224) 320)
                                        >> hline (-80)
                                        >> rlineto (vec 112 (-160))
                                        >> rlineto (vec (-112) (-160))

path03 :: Path Double
path03 = execPath (P2 384 96) $ hline 96 >> vline 56 >> hline (-136) 

path04 :: Path Double
path04 = execPath (P2 328 192) $ hline 152 >> vline 56 >> hline (-192) 

