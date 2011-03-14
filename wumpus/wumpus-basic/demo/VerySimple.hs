{-# OPTIONS -Wall #-}


module VerySimple where

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
    drawi_ $ miniDisk `at` P2 120 0
    drawi_ $ miniDisk `at` P2 80  0
    drawi_ $ (uconvert miniDiskEm) `at` P2 40  0
    drawi_ $ (uconvert miniDiskEn) `at` P2 0   0


miniDisk :: InterpretUnit u => LocGraphic u
miniDisk = local_ctx (fill_colour sienna) $ filledDisk 3


-- Should be the same size as 3 at 24 Point...
--
miniDiskEm :: LocGraphic Em
miniDiskEm = local_ctx (fill_colour aquamarine) $ filledDisk 0.125

-- Should be the same size as 3 at 24 Point...
--
miniDiskEn :: LocGraphic En
miniDiskEn = local_ctx (fill_colour red) $ filledDisk 0.25


sienna :: RGBi
sienna = RGBi 160 82 45

aquamarine :: RGBi
aquamarine = RGBi 127 255 212