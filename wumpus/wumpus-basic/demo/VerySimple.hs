{-# OPTIONS -Wall #-}


module VerySimple where

-- import Wumpus.Basic.Geometry
import Wumpus.Basic.Kernel


import Wumpus.Core                      -- package: wumpus-core
import Wumpus.Core.Colour ( red )

import System.Directory



main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/"
    writeEPS "./out/very_simple01.eps" pic1
    writeSVG "./out/very_simple01.svg" pic1


std_ctx :: DrawingContext
std_ctx = standardContext 24




--------------------------------------------------------------------------------

pic1 :: Picture
pic1 = runCtxPictureU std_ctx $ drawTracing mf



mf :: TraceDrawing Double ()
mf = do
    draw $ miniDisk `at` P2 120 0
    draw $ rotate30 $ miniDisk `at` P2 80  0
    draw $ (uconvF miniDiskEm) `at` P2 40  0
    draw $ (uconvF miniDiskEn) `at` P2 0   0


miniDisk :: InterpretUnit u => LocGraphic u
miniDisk = localize (fill_colour sienna) $ dcDisk FILL 3


-- Should be the same size as 3 at 24 Point...
--
miniDiskEm :: LocGraphic Em
miniDiskEm = localize (fill_colour aquamarine) $ dcDisk FILL 0.125

-- Should be the same size as 3 at 24 Point...
--
miniDiskEn :: LocGraphic En
miniDiskEn = localize (fill_colour red) $ dcDisk FILL 0.25


sienna :: RGBi
sienna = RGBi 160 82 45

aquamarine :: RGBi
aquamarine = RGBi 127 255 212

