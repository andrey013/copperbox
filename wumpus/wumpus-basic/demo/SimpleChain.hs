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
    drawl (P2 0 0) $ runChainH 70 $ 
                       mapM onChain [text01, minidisk, text02, minidisk]
                               
                               

    drawl (P2 0 0) $ localize (fill_colour sienna) $ dcRectangle DRAW_FILL 4 4

    drawl (P2 0 200) $ runTableRight 6 (30,18) $ mapM onChain $ diskList 32
                                
                                

    drawl (P2 300 100) $ dcDisk DRAW_FILL 6
    drawl (P2 300 100) $ runChain pchain_start $ pchain $ intList 30


pchain_start :: ChainScheme Double 
pchain_start = radialChain 50 half_pi (pi/8)



pchain :: InterpretUnit u => [LocGraphic u] -> Chain u (UNil u)
pchain gs = do 
    ys <- ntimes 12 gs
    setChainScheme (horizontalChainScm 20)
    zs <- ntimes 15 ys
    setChainScheme (verticalChainScm 20)
    mapM_ onChain zs
    return UNil
  where  
    ntimes n (x:xs) | n > 0 = onChain x >> ntimes (n-1) xs
    ntimes _ xs             = return xs



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

