{-# OPTIONS -Wall #-}

module ChainDemo where

import Wumpus.Drawing.Colour.SVGColours

import Wumpus.Basic.Kernel              -- package: wumpus-basic

import Wumpus.Core                      -- package: wumpus-core

import Control.Applicative
import System.Directory


main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/"
    let pic1 = runCtxPictureU std_attr chain_pic
    writeEPS "./out/chain_demo.eps" pic1
    writeSVG "./out/chain_demo.svg" pic1


std_attr :: DrawingContext
std_attr = fill_colour rosy_brown $ standardContext 12



chain_pic :: CtxPicture
chain_pic = drawTracing $ do 
    drawl (zeroPt) $ snapGridX >>= \w -> 
                     runChain (mapM onChain [dot1, dot1, dot1]) (chainH w) 

    drawl (P2 100 0) $ runChain (mapM onChain [dot2, dot1, dot2, dot1, dot2])
                                (radialChain 60 (pi*0.25) (d2r (30::Double)) )
                              
    drawl (P2 0 200) $ runChain (mapM (onChain . dot) 
                                      [bisque, gray, khaki, khaki, bisque, gray])
                                (chainH 60)


 

dot1 :: DLocGraphic
dot1 = dot red

dot2 :: DLocGraphic
dot2 = dot thistle

dot :: RGBi -> DLocGraphic 
dot rgb = localize (fill_colour rgb) $ dcDisk FILL 6


snapGridX :: (DrawingCtxM m, Fractional u) => m u
snapGridX = vector_x <$> snapmove (1,1)



