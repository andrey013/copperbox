{-# OPTIONS -Wall #-}

module Basic where

import Wumpus.Core.Line
import Wumpus.Core.Point
import Wumpus.Core.Polygon

import Wumpus.Drawing.Path
import Wumpus.Drawing.Picture
import Wumpus.Drawing.SVGColours


demo1 :: IO ()
demo1 = writePicture "basic1.ps" (displace 60 380 drawing1) where
  drawing1 =    (picPath stroke $ straightLine $ LS (P2 0 0) (P2 0 150))
           <..> (picPath stroke $ straightLine $ LS (P2 0 0) (P2 150 0))
           <..> (picCircle 15 `at` (5,5))
           <..> (withGray 0.5 $ picDisk 15 `at` (60,5))
           <..> (withRgbColour khaki $ 
                   picPolygon fill $ 
                   Polygon [P2 140 5, P2 140 30, P2 170 5, P2 170 30])
           <..> (diamond 30 30 stroke `at` (10,50))
           <..> (picDisk 2 `at` (10,50))
           <..> (diamond 30 20 stroke `at` (80,50))


demo2 :: IO ()
demo2 = writePicture "basic2.ps" (displace 50 300 (pic1 <++> pic2))

pic1 :: Picture
pic1 = vcat [dia1,dia2,dia3,dia4]
  where
    dia1 = withRgbColour blueViolet    $ diamond 40 40 stroke
    dia2 = withRgbColour darkSeaGreen  $ diamond 40 40 stroke
    dia3 = withRgbColour darkSeaGreen  $ diamond 20 20 stroke
    dia4 = withRgbColour darkSeaGreen  $ diamond 15 15 stroke




pic2 :: Picture
pic2 = (dia1 <//> dia2)
  where
    dia1 = withRgbColour blueViolet    $ diamond 40 40 stroke
    dia2 = withRgbColour darkSeaGreen  $ diamond 30 30 stroke

demo3 :: IO ()
demo3 = writePicture "basic3.ps" (displace 50 300 dots)
  where
    dots = dotX <++> dotPlus <++> dotAsterisk <++> dotSquare <++> dotPentagon
                <++> dotTriangle <++> dotDiamond

demo4 :: IO ()
demo4 = writePicture "basic4.ps" (displace 10 300 squares)
  where
    squares = picPolygon stroke (square 10 zeroPt) 
         <++> picPolygon stroke (square 20 zeroPt)
         <++> picPolygon stroke (square 30 zeroPt)
         <++> picPolygon stroke (square 40 zeroPt)
         <++> picPolygon stroke (square 50 zeroPt)
         <//> innerSquares
         <//> cattySquares
    innerSquares = (withRgbColour blueViolet $ picPolygon stroke (square 50 zeroPt))
        `centered` (withRgbColour darkSeaGreen $ picPolygon stroke (square 30 zeroPt))
    
    cattySquares = hcat $ replicate 8 (picPolygon stroke $ square 20 zeroPt)