{-# OPTIONS -Wall #-}


module SimpleChain where

import Wumpus.Basic.Kernel

import Wumpus.Core                      -- package: wumpus-core
import Wumpus.Core.Colour

import Data.List ( unfoldr )
import System.Directory

main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/"
    let pic1 = runCtxPictureU std_attr drawing01
    writeEPS "./out/simple_chain01.eps" pic1
    writeSVG "./out/simple_chain01.svg" pic1


std_attr :: DrawingContext
std_attr = standardContext 24


drawing01 :: CtxPicture
drawing01 = drawTracing mf 



-- Although TraceDrawing can be fully unit polymorphic, it seems 
-- always best to specialize as we are stating concrete values
-- (and they will be in some unit).
--

mf :: TraceDrawing Double ()
mf = do
    drawl (P2 0 0) $ chain_ (chainH 70) [text01, minidisk, text02, minidisk]
    drawl (P2 0 0) $ localize (fill_colour sienna) $ filledRectangle 4 4

    drawl (P2 0 200) $ chain (tableDown 6 (30,18)) $ diskList 30

    drawl (P2 300 100) $ chain (chainCircle 50 0 (pi/8)) $ diskList 15


-- Normally, text calculate the advance vector from the font 
-- metrics...
--
text01 :: LocGraphic Double
text01 = plainTextLine "T01"
    

text02 :: LocGraphic Double
text02 = plainTextLine "T02"



minidisk :: LocGraphic Double
minidisk = moveStart (displaceV 7) $ filledDisk 6

diskList :: Int -> [LocGraphic Double]
diskList n = take n $ unfoldr phi black
  where
    phi rgb@(RGBi r g b) = let nrgb = RGBi (r+8) (g+8) (b+8)
                           in Just (localize (fill_colour rgb) minidisk, nrgb)

sienna :: RGBi
sienna = RGBi 160 82 45

-- NOTE - next in AdvGrphic is purloining of a good name.