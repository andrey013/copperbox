{-# OPTIONS -Wall #-}

module PathRel where

import Wumpus.Drawing.Colour.SVGColours
import Wumpus.Drawing.Paths.Relative
import Wumpus.Drawing.Paths.RelativeConstruction

import qualified Wumpus.Drawing.Paths.Absolute as A

import Wumpus.Basic.Kernel                      -- package: wumpus-basic
import Wumpus.Core                              -- package: wumpus-core

import Data.AffineSpace

import System.Directory


main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/"
    let pic1 = runCtxPictureU (standardContext 18) path_pic
    writeEPS "./out/path_rel.eps" pic1
    writeSVG "./out/path_rel.svg" pic1 

path_pic :: CtxPicture
path_pic = drawTracing $ do
    drawl zeroPt       $ path1
    drawl (P2 100 0)   $ path2
    return ()  
    

path1 :: LocGraphic Double
path1 = strokeRelPath $ lineTo (hvec 20)

path2 :: LocImage A.AbsPath Double
path2 = evalPath [ move (0,20) , move (20,0) , insert disk1 , move (0, (-20)) ]
  where
    disk1 = strokedDisk 2