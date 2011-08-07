{-# OPTIONS -Wall #-}


module SimpleChain where

import Wumpus.Basic.Kernel

import Wumpus.Core                      -- package: wumpus-core
import Wumpus.Core.Colour

import Data.List ( unfoldr )
import Data.Monoid
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
    drawl (P2 0 0) $ runChain_ (mapM cnext [text01, minidisk, text02, minidisk])
                               (chainH 70) 
                               

    drawl (P2 0 0) $ localize (fill_colour sienna) $ dcRectangle FILL 4 4

    drawl (P2 0 200) $ runChain (mapM cnext $ diskList 32)
                                (tableRight 6 (30,18)) 
                                

    drawl (P2 300 100) $ runChain (pchain $ diskList 30) pchain_start 


pchain_start :: ChainScheme Double 
pchain_start = radialChain 50 (0.5*pi) (pi/8)



pchain :: InterpretUnit u => [LocGraphic u] -> Chain u (UNil u)
pchain xs = do 
    ys <- ntimes 12 xs
    setChainScheme (chainH 20)
    zs <- ntimes 15 ys
    setChainScheme (chainV 20)
    mapM_ cnext zs
    return UNil
  where  
    ntimes n (x:xs) | n > 0 = cnext x >> ntimes (n-1) xs
    ntimes _ xs             = return xs



-- Normally, text calculate the advance vector from the font 
-- metrics...
--
text01 :: LocGraphic Double
text01 = dcTextlabel "T01"
    

text02 :: LocGraphic Double
text02 = dcTextlabel "T02"



minidisk :: LocGraphic Double
minidisk = moveStart (vvec 7) $ dcDisk FILL 6

diskList :: Int -> [LocGraphic Double]
diskList n = take n $ unfoldr phi black
  where
    phi rgb@(RGBi r g b) = let nrgb = RGBi (r+8) (g+8) (b+8)
                           in Just (localize (fill_colour rgb) minidisk, nrgb)

sienna :: RGBi
sienna = RGBi 160 82 45

