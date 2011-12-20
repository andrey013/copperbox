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
    drawl (P2 0 0) $ runChain (horizontalScheme 70) $ 
                       chainMany [text01, minidisk, text02, minidisk]
                               
                               

    drawl (P2 0 0) $ localize (fill_colour sienna) $ dcRectangle DRAW_FILL 4 4

    drawl (P2 0 200) $ distribRowwiseTable 6 (30,18) $ diskList 32
                                
                                

    drawl (P2 300 100) $ dcDisk DRAW_FILL 6
    drawl (P2 300 100) $ runChain counting_scm $ chainMany (intList 30)
    drawl (P2 400 100) $ runChain (catTrailScheme $ sawtoothWave 20 30 0)
                                  (chainReplicate 20 minidisk)


counting_scm :: (InterpretUnit u, Floating u) => ChainScheme u
counting_scm = countingScheme [ (12, radialChainScm 50 half_pi (pi/8))
                              , (15, horizontalScheme 20)
                              ]
                              (verticalScheme 20)



-- Normally, text calculate the advance vector from the font 
-- metrics...
--
text01 :: LocGraphic Double
text01 = dcTextlabel "T01"
    

text02 :: LocGraphic Double
text02 = dcTextlabel "T02"



minidisk :: LocGraphic Double
minidisk = moveStart (vvec 7) $ dcDisk DRAW_FILL 6

diskList :: Int -> [LocGraphic Double]
diskList n = take n $ unfoldr phi black
  where
    phi rgb@(RGBi r g b) = let nrgb = RGBi (r+8) (g+8) (b+8)
                           in Just (localize (fill_colour rgb) minidisk, nrgb)

intList :: Int  -> [LocGraphic Double]
intList n = take n $ map fn $ cycle [1,2,3,4,5,6,7,8,9,0::Int]
  where
    fn = runPosObject CENTER . posTextUpright . show

sienna :: RGBi
sienna = RGBi 160 82 45

