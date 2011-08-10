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


-- Note - currently the code is quite messy, path drawing needs 
-- some convenience combinators.

clip_pic :: CtxPicture
clip_pic = drawTracing $ localize (fill_colour medium_slate_blue) $ do
    drawl (P2   0 320) $ (drawClosedPath FILL =<< zapLocQ (extrPathSpec path01))
    drawl (P2 112 320) $ localize (fill_colour powder_blue) $ 
                         (drawClosedPath FILL =<< zapLocQ (extrPathSpec path02))
    drawl (P2 384 416) $ (drawClosedPath FILL =<< zapLocQ (extrPathSpec path03))
    drawl (P2 328 512) $ (drawClosedPath FILL =<< zapLocQ (extrPathSpec path04))
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

-- | This is one for Wumpus-Basic - the set of combinators to 
-- shift between Images and Queries needs sorting out
--

zapLocQ :: InterpretUnit u => LocQuery u a-> LocImage u a
zapLocQ ma = promoteLoc $ \pt -> 
   askDC >>= \ctx ->
   let a = runLocQuery ma ctx pt
   in return a

clip1 :: LocGraphic Double
clip1 = zapLocQ (extrPathSpec path01) >>= \a -> 
        ignoreAns $ locClip a (background black)
  
clip2 :: LocGraphic Double
clip2 = zapLocQ (extrPathSpec path02) >>= \a -> 
        locClip a (background medium_violet_red)

clip3 :: LocGraphic Double
clip3 = zapLocQ (extrPathSpec path03) >>= \a -> locClip a (background black)

clip4 :: LocGraphic Double
clip4 = zapLocQ (extrPathSpec path04) >>= \a -> locClip a (background black)


iheartHaskell :: LocGraphic Double
iheartHaskell = promoteLoc $ \pt -> 
    let body  = dcTextlabel "I Haskell" `at` pt
        heart = localize (set_font symbol) $ 
                  dcTextlabel "&heart;" `at` (pt .+^ hvec 7)
    in body `mappend` heart

-- Note - this needs a more uniform name. In Wumpus an object 
-- that produces a @UNil@ is called a __Graphic but that 
-- convention seems misleading here.
--
type UPathSpec u = PathSpec u (UNil u)

-- zeroPt
path01 :: UPathSpec Double
path01 =  do 
    hlineto 80 
    lineto (vec 112 160) 
    lineto (vec (-112) 160)
    hlineto (-80)
    lineto (vec 112 (-160))
    lineto (vec (-112) (-160))
    ureturn 


-- (P2 112 0)
path02 :: UPathSpec Double
path02 = do 
    hlineto 80 
    lineto (vec 72 112)
    lineto (vec 72 (-112))
    hlineto 80
    lineto (vec (-224) 320)
    hlineto (-80)
    lineto (vec 112 (-160))
    lineto (vec (-112) (-160))
    ureturn

-- (P2 384 96) 
path03 :: UPathSpec Double
path03 = hlineto 96 >> vlineto 56 >> hlineto (-136) >> ureturn

-- (P2 328 192)
path04 :: UPathSpec Double
path04 = hlineto 152 >> vlineto 56 >> hlineto (-192) >> ureturn

