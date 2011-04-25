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
turtle_pic = udrawTracing (0::Double) $ localize (snap_grid_factors 40 40) $ do 
    draw $ dcRectangle FILL 40 10 `at` zeroPt
    runTurtleT (0,0) $ 
      moveUp >> moveUp >> moveUp >> tnode (dcTextlabel "up3") >>  
      moveRight >> moveRight >> tnode (dcTextlabel "right2")
      
tnode :: ( Fractional u, InterpretUnit u, DrawingCtxM m, TraceM m, TurtleM m
         , u ~ MonUnit (m()) )
      => LocImage u a -> m ()
tnode img = getLoc >>= \coord -> position coord >>= \pt -> drawl pt img
        


