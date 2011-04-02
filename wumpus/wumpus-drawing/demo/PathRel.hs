{-# OPTIONS -Wall #-}

module PathRel where

import Wumpus.Drawing.Colour.SVGColours
import Wumpus.Drawing.Paths.Relative
import Wumpus.Drawing.Paths.RelativeConstruction

import qualified Wumpus.Drawing.Paths.Absolute as A

import Wumpus.Basic.Kernel                      -- package: wumpus-basic
import Wumpus.Core                              -- package: wumpus-core


import System.Directory


main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/"
    let pic1 = runCtxPictureU (standardContext 18) path_pic
    writeEPS "./out/path_rel.eps" pic1
    writeSVG "./out/path_rel.svg" pic1 

path_pic :: CtxPicture
path_pic = drawTracing $ do
    drawl (P2 0 0)   $ path1
    drawl (P2 0 150) $ path2
    return ()  
    

path0 :: LocGraphic Double
path0 = strokeRelPath $ lineTo (hvec 20)

path1 :: LocImage A.AbsPath Double
path1 = localize (stroke_colour red) $ strokePathSpec path_spec1

path2 :: LocImage A.AbsPath Double
path2 = localize (stroke_colour red) $ fillPathSpec path_spec1


path_spec1 :: PathSpec Double
path_spec1 =  
    [ (id, [ move (0,50) 
           , move (50,0) 
           , insert disk1 
           , pen_up
           , move (0, (-50))
           , pen_down 
           , move (100,0) 
           ])
    , (stroke_colour blue, 
           [ move (50,0)
           , move (0,50)
           ])
    ]
  where
    disk1 = strokedDisk 10