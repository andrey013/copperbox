{-# OPTIONS -Wall #-}

module ChainDemo where

import Wumpus.Drawing.Chains
import Wumpus.Drawing.Colour.SVGColours
import Wumpus.Drawing.Extras.Grids

import Wumpus.Basic.Kernel              -- package: wumpus-basic

import Wumpus.Core                      -- package: wumpus-core

import Data.AffineSpace                         -- package: vector-space
import Data.VectorSpace

import Control.Applicative
import System.Directory


-- WARNING - interoirGRid seems to be drawing exteriors as well...

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
    drawli_ zeroPt $ snapGridX >>= \w -> 
                    chainDisplace (displaceH w) [dot1, dot1, dot1]

    drawli_ (P2 100 0) $ chainRadial 60 (pi*0.25) (d2r (30::Double) )
                                        [dot2, dot1, dot2, dot1, dot2]


    drawli_ (P2 0 200) $ apChainIterateH 60 dot 
                                       [bisque, gray, khaki, khaki, bisque, gray]

    drawl zeroPt $ grid (5,4) blue

                                    
{-
-- SOLVED.
    
-- How onerous is the @empty__@ argument? 
-- Obviously @empty__@ names are already long...

locGraphicDistrib :: Num u 
                  => PointDisplace u -> [LocGraphic u] -> LocGraphic u
locGraphicDistrib fn = distribute fn
-}
 

dot1 :: DLocGraphic
dot1 = dot red

dot2 :: DLocGraphic
dot2 = dot thistle

dot :: RGBi -> DLocGraphic 
dot rgb = localize (fill_colour rgb) $ filledDisk 6


snapGridX :: (DrawingCtxM m, Fractional u) => m u
snapGridX = vector_x <$> snapmove (1,1)




apChainIterateH :: InterpretUnit u
                => u -> (a -> LocGraphic u) -> [a] -> LocImage Point2 u
apChainIterateH dx = apChainIterate (^+^ hvec dx)  (\s pt -> pt .+^ s) (V2 0 0)





{-
-- Can write an analogy to distribute but cannot re-implement it
-- with genIterate.
--
distribute' :: Num u
           => Vec2 u -> [LocGraphic u] -> LocImage u (Point2 u)
distribute' v = genIterate (^+^ v)  (\s pt -> pt .+^ s) (V2 0 0)


-}

