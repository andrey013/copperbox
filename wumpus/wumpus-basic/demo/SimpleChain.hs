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
    drawl (P2 0 0) $ localize (fill_colour sienna) $ dcRectangle FILL 4 4

    drawl (P2 0 200) $ chain (tableRight 6 (30,18)) $ diskList 32

    drawl (P2 300 100) $ chain pchain $ diskList 30



pchain :: ChainAlg Double
pchain = prefix 12 (radialChain 50 (0.5*pi) (pi/8)) 
       $ prefix 15 (chainH 20) 
       $ chainV 20


-- Normally, text calculate the advance vector from the font 
-- metrics...
--
text01 :: LocGraphic Double
text01 = dcTextlabel "T01"
    

text02 :: LocGraphic Double
text02 = dcTextlabel "T02"



minidisk :: LocGraphic Double
minidisk = moveStart (displaceV 7) $ dcDisk FILL 6

diskList :: Int -> [LocGraphic Double]
diskList n = take n $ unfoldr phi black
  where
    phi rgb@(RGBi r g b) = let nrgb = RGBi (r+8) (g+8) (b+8)
                           in Just (localize (fill_colour rgb) minidisk, nrgb)

sienna :: RGBi
sienna = RGBi 160 82 45

