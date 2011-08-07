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
import Wumpus.Drawing.Extras.Clip
import Wumpus.Drawing.Paths.Relative
import Wumpus.Drawing.Text.StandardFontDefs

import Wumpus.Basic.Kernel                      -- package: wumpus-basic

import Wumpus.Core                              -- package: wumpus-core

import Data.AffineSpace                         -- package: vector-space

import Data.Monoid
import System.Directory


main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/"
    let pic1 = runCtxPictureU std_ctx clip_pic
    writeEPS "./out/clip_pic.eps" pic1
    writeSVG "./out/clip_pic.svg" pic1


std_ctx :: DrawingContext
std_ctx = standardContext 14


clip_pic :: CtxPicture
clip_pic = drawTracing $ localize (fill_colour medium_slate_blue) $ do
    drawl (P2   0 320) $ drawClosedPath FILL path01
    drawl (P2 112 320) $ localize (fill_colour powder_blue) $ drawClosedPath FILL path02
    drawl (P2 384 416) $ drawClosedPath FILL path03
    drawl (P2 328 512) $ drawClosedPath FILL path04
    drawl (P2   0   0) $ clip1
    drawl (P2 112   0) $ clip2
    drawl (P2 384  96) $ clip3
    drawl (P2 328 192) $ clip4


background :: RGBi -> LocGraphic Double
background rgb = promoteLoc $ \_ -> 
    ignoreAns $ localize (text_colour rgb) $ ihh `at` P2 0 288
  where
    ihh = runChain (mapM cnext $ replicate 112 iheartHaskell) 
                   (tableDown 18 (86,16)) 



clip1 :: LocGraphic Double
clip1 = locClip path01 $ background black
  
clip2 :: LocGraphic Double
clip2 = locClip path02 $ background medium_violet_red

clip3 :: LocGraphic Double
clip3 = locClip path03 $ background black

clip4 :: LocGraphic Double
clip4 = locClip path04 $ background black


iheartHaskell :: LocGraphic Double
iheartHaskell = promoteLoc $ \pt -> 
    let body  = dcTextlabel "I Haskell" `at` pt
        heart = localize (set_font symbol) $ 
                  dcTextlabel "&heart;" `at` (pt .+^ hvec 7)
    in body `mappend` heart


-- zeroPt
path01 :: RelPath Double
path01 = evalPathSpec $  hlineto 80 
                      >> lineto (vec 112 160) 
                      >> lineto (vec (-112) 160)
                      >> hlineto (-80)
                      >> lineto (vec 112 (-160))
                      >> lineto (vec (-112) (-160))
 
-- (P2 112 0)
path02 :: RelPath Double
path02 = evalPathSpec  $  hlineto 80 
                       >> lineto (vec 72 112)
                       >> lineto (vec 72 (-112))
                       >> hlineto 80
                       >> lineto (vec (-224) 320)
                       >> hlineto (-80)
                       >> lineto (vec 112 (-160))
                       >> lineto (vec (-112) (-160))

-- (P2 384 96) 
path03 :: RelPath Double
path03 = evalPathSpec $ hlineto 96 >> vlineto 56 >> hlineto (-136) 

-- (P2 328 192)
path04 :: RelPath Double
path04 = evalPathSpec  $ hlineto 152 >> vlineto 56 >> hlineto (-192) 

