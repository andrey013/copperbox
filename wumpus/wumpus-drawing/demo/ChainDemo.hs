{-# OPTIONS -Wall #-}

module ChainDemo where

import Wumpus.Drawing.Chains.Base
import Wumpus.Drawing.Colour.SVGColours
import Wumpus.Drawing.Dots.AnchorDots


import Wumpus.Basic.Kernel              -- package: wumpus-basic

import Wumpus.Core                      -- package: wumpus-core

import Data.AffineSpace                 -- package: vector-space

import System.Directory

main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/"
    let pic1 = runCtxPictureU std_attr chain_pic
    writeEPS "./out/chain_demo.eps" pic1
    writeSVG "./out/chain_demo.svg" pic1


std_attr :: DrawingContext
std_attr = fill_colour rosy_brown $ standardContext 12



chain_pic :: DCtxPicture
chain_pic = drawTracing mf 



mf :: (Floating u, FromPtSize u) => TraceDrawing u ()
mf = mapM_ (\pt -> drawi $ dotDisk `at` pt) column_01


-- Note - infinite lists are sometimes /bad/ - okay to zip along 
-- but bad to map on.
-- 
-- A distinction would be good.
--
column_01 :: Num u => [Point2 u]
column_01 = take 10 $ iterate (.+^ vvec (-16)) (P2 0 600)

