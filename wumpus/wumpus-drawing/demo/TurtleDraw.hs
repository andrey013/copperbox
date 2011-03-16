{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

module TurtleDraw where

import Wumpus.Drawing.Extras.Turtle.TurtleMonad

import Wumpus.Basic.Kernel                      -- package: wumpus-basic
import Wumpus.Core                              -- package: wumpus-core

import System.Directory


main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/"
    let pic1 = runCtxPictureU (standardContext 14) turtle_pic
    writeEPS "./out/turtle_draw01.eps" pic1
    writeSVG "./out/turtle_draw01.svg" pic1


turtle_pic :: CtxPicture
turtle_pic = udrawTracing (0::Double) $ do 
    draw $ filledRectangle 40 10 `at` zeroPt
    runTurtleT (0,0) (coordinateScaling 14 14) $ 
      moveUp >> moveUp >> moveUp >> tnode (textline "up3") >>  
      moveRight >> moveRight >> tnode (textline "right2")
      
tnode :: ( Fractional u, InterpretUnit u, DrawingCtxM m, TraceM m, TurtleM m
         , u ~ DUnit (m()) )
      => LocGraphic u -> m ()
tnode gf = getLoc >>= \coord -> position coord >>= \pt -> drawl pt gf
        


