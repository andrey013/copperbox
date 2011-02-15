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
import Wumpus.Drawing.Text.SafeFonts

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


top_pic :: DCtxPicture
top_pic = drawTracing $ localize (fill_colour medium_slate_blue) $ do
    draw $ filledPath $ toPrimPath path01
    draw $ localize (fill_colour powder_blue) $ filledPath $ toPrimPath path02
    draw $ filledPath $ toPrimPath path03
    draw $ filledPath $ toPrimPath path04

clip_pic :: DCtxPicture
clip_pic = drawTracing $ do
    mapM_ draw $ [ clip1, clip2, clip3, clip4 ]


background :: RGBi -> DGraphic
background rgb = 
    localize (text_colour rgb) $ ihh `at` P2 0 288
  where
    ihh = unchain 112 emptyLocGraphic iheartHaskell $ tableDown 18 (86,16)

-- Wumpus-Basic needs a clip function, but is this the most 
-- satisfactory definition?
--
clipGraphic :: (Num u, Ord u) => PrimPath u -> Graphic u -> Graphic u 
clipGraphic cp = fmap (bimapR (metamorphPrim (clip cp)))


clip1 :: DGraphic
clip1 = clipGraphic (toPrimPath path01) (background black)
  
clip2 :: DGraphic
clip2 = clipGraphic (toPrimPath path02) (background medium_violet_red)

clip3 :: DGraphic 
clip3 = clipGraphic (toPrimPath path03) (background black)

clip4 :: DGraphic 
clip4 = clipGraphic (toPrimPath path04) (background black)


iheartHaskell :: Num u => FromPtSize u => LocGraphic u
iheartHaskell = promoteR1 $ \pt -> 
    let body  = textline "I Haskell" `at` pt
        heart = localize (set_font symbol) $ 
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

