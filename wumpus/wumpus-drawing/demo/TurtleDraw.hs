{-# LANGUAGE FlexibleContexts           #-}
{-# OPTIONS -Wall #-}

module TurtleDraw where

import Wumpus.Basic.Kernel
import Wumpus.Drawing.Dots.Marks
import Wumpus.Drawing.Turtle.TurtleMonad

import Wumpus.Core                      -- package: wumpus-core

import System.Directory


main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/"
    let pic1 = runCtxPictureU (standardContext 14) turtle_pic
    writeEPS "./out/turtle_draw01.eps" pic1
    writeSVG "./out/turtle_draw01.svg" pic1


turtle_pic :: DCtxPicture
turtle_pic = drawTracing $ do 
    draw $ filledRectangle 40 10 `at` zeroPt
    runTurtleT (0,0) (coordinateScaling 14 14) $ 
      moveUp >> moveUp >> moveUp >> node (textline "up3") >>  
      moveRight >> moveRight >> node (textline "right2")
      
  
        
connect' :: Num u => Point2 u -> Point2 u -> Graphic u
connect' a b = openStroke $ vertexPath [a,b]  


