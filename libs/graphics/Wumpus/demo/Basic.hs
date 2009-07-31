{-# OPTIONS -Wall #-}

module Basic where


import Wumpus.Core.Line
import Wumpus.Core.Point
import Wumpus.Core.Polygon

import Wumpus.Drawing.Basic
import Wumpus.Drawing.SVGColours


demo1 :: IO ()
demo1 = writePicture "basic1.ps" (displace 60 380 drawing1) where
  drawing1 =    (picLine $ lineTo (P2 0 0) (P2 0 150))
           <..> (picLine $ lineTo (P2 0 0) (P2 150 0))
           <..> (picCircle $ circle (5,5) 15)
           <..> (withGray 0.5 $ picDisk $  disk (60,5) 15)
           <..> (picPolygon $ Polygon [P2 140 5, P2 140 30, P2 170 5, P2 170 30])
           <..> (picPolygon $ diamond 30 30 (P2 10 50))
           <..> (picDisk $ disk (10,50) 2)
           <..> (picPolygon $ diamond 30 20 (P2 80 50))


demo2 :: IO ()
demo2 = writePicture "basic2.ps" (displace 50 300 (pic1 <++> pic2))

pic1 :: Picture
pic1 = vcat [dia1,dia2,dia3,dia4]
  where
    dia1 = withRgbColour blueViolet    $ (picPolygon $ diamond 40 40 zeroPt)
    dia2 = withRgbColour darkSeaGreen  $ (picPolygon $ diamond 40 40 zeroPt)
    dia3 = withRgbColour darkSeaGreen  $ (picPolygon $ diamond 20 20 zeroPt)
    dia4 = withRgbColour darkSeaGreen  $ (picPolygon $ diamond 15 15 zeroPt)




pic2 :: Picture
pic2 = (dia1 <//> dia2)
  where
    dia1 = withRgbColour blueViolet    $ (picPolygon $ diamond 40 40 zeroPt)
    dia2 = withRgbColour darkSeaGreen  $ (picPolygon $ diamond 30 30 zeroPt)

demo3 :: IO ()
demo3 = writePicture "basic3.ps" (displace 50 300 dots)
  where
    dots = dotX <++> dotPlus <++> dotAsterisk <++> dotSquare <++> dotPentagon
                <++> dotTriangle <++> dotDiamond

demo4 :: IO ()
demo4 = writePicture "basic4.ps" (displace 10 300 squares)
  where
    squares = picPolygon (square 10 zeroPt) 
         <++> picPolygon (square 20 zeroPt)
         <++> picPolygon (square 30 zeroPt)
         <++> picPolygon (square 40 zeroPt)
         <++> picPolygon (square 50 zeroPt)
         <//> innerSquares
         <//> cattySquares
    innerSquares = (withRgbColour blueViolet $ picPolygon (square 50 zeroPt))
        `centered` (withRgbColour darkSeaGreen $ picPolygon (square 30 zeroPt))
    
    cattySquares = hcat $ replicate 8 (picPolygon $ square 20 zeroPt)