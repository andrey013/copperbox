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
import Wumpus.Drawing.Paths
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
    drawl (P2   0 320) $ runPathSpec_ CFILL path01
    drawl (P2 112 320) $ localize (fill_colour powder_blue) $ 
                           runPathSpec_ CFILL path02
    drawl (P2 384 416) $ runPathSpec_ CFILL path03
    drawl (P2 328 512) $ runPathSpec_ CFILL path04
    drawl (P2   0   0) $ clip1
    drawl (P2 112   0) $ clip2
    drawl (P2 384  96) $ clip3
    drawl (P2 328 192) $ clip4


-- PathSpec is overkill here...
--
extrPathSpec :: InterpretUnit u 
             => PathSpec u a -> LocQuery u (AbsPath u)
extrPathSpec spec = qpromoteLoc $ \pt ->
    fmap post $ qapplyLoc (stripGenPathSpec () CSTROKE spec) pt
  where
    post (_,_,c) = c

background :: RGBi -> LocGraphic Double
background rgb = promoteLoc $ \_ -> 
    ignoreAns $ localize (text_colour rgb) $ ihh `at` P2 0 288
  where
    ihh = distribColumnwiseTable 18 (86,16) $ replicate 112 iheartHaskell
                   

-- | This is one for Wumpus-Basic - the set of combinators to 
-- shift between Images and Queries needs sorting out
--

zapLocQ :: InterpretUnit u => LocQuery u a-> LocImage u a
zapLocQ ma = promoteLoc $ \pt -> 
   askDC >>= \ctx ->
   let a = runLocQuery ctx pt ma in return a

clip1 :: LocGraphic Double
clip1 = zapLocQ (extrPathSpec path01) >>= \a -> 
        locClip a (background black)
  
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
path01 = do 
    hpenline 80 
    penline (vec 112 160) 
    penline (vec (-112) 160)
    hpenline (-80)
    penline (vec 112 (-160))
    penline (vec (-112) (-160))
    ureturn


-- (P2 112 0)
path02 :: UPathSpec Double
path02 = do 
    hpenline 80 
    penline (vec 72 112)
    penline (vec 72 (-112))
    hpenline 80
    penline (vec (-224) 320)
    hpenline (-80)
    penline (vec 112 (-160))
    penline (vec (-112) (-160))
    ureturn

-- (P2 384 96) 
path03 :: UPathSpec Double
path03 = hpenline 96 >> vpenline 56 >> hpenline (-136) >> ureturn

-- (P2 328 192)
path04 :: UPathSpec Double
path04 = hpenline 152 >> vpenline 56 >> hpenline (-192) >> ureturn

