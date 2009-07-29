{-# OPTIONS -Wall #-}

module Basic where


import Wumpus.Core.Line
import Wumpus.Core.Point
import Wumpus.Core.Polygon

import Wumpus.Drawing.Basic
import Wumpus.Drawing.PostScript
import Wumpus.Drawing.SVGColours


demo1 :: IO ()
demo1 = writePS "basic1.ps" $ runWumpus env0 $ drawing1 where
  drawing1 = do { ps_translate 60 380 
                ; drawLine $ lineTo (P2 0 0) (P2 0 150)
                ; drawLine $ lineTo (P2 0 0) (P2 150 0)
                ; drawCircle $ circle (5,5) 15
                ; withGray 0.5 $ drawDisk $  disk (60,5) 15
--                ; wedge (100,5) 15 0 60
--                ; withGray 0 $ ellipse (60,5) (15,10)
--                ; ellipticarc (120,5) (15,10) 0 270
                ; fillPolygon $ Polygon [P2 140 5, P2 140 30, P2 170 5, P2 170 30]
                ; strokePolygon $ diamond 30 30 (P2 10 50)
                ; drawDisk $ disk (10,50) 2
                -- Properties currently not implemented ...
                -- lineColour blueViolet $ lineWidth 1 $ fillColour burlywood
                ; strokePolygon $ diamond 30 20 (P2 80 50) 
       --         ; withRgbColour blueViolet $ drawPolygon $ dotDiamond (P2 180 50)
                }

demo2 :: IO ()
demo2 = writePS "basic2.ps" (psDraw $ (pic1 <+> pic2) `place` (P2 50 300) )

pic1 :: Picture
-- pic1 = (dia4 `below` dia3 `below` dia2 `below` dia1 )
pic1 = vcat [dia1,dia2,dia3,dia4]
  where
    dia1 = picColour blueViolet    $ (picPolygon $ diamond 40 40)
    dia2 = picColour darkSeaGreen  $ (picPolygon $ diamond 40 40)
    dia3 = picColour darkSeaGreen  $ (picPolygon $ diamond 20 20)
    dia4 = picColour darkSeaGreen  $ (picPolygon $ diamond 15 15)




pic2 :: Picture
pic2 = (dia1 </> dia2)
  where
    dia1 = picColour blueViolet    $ (picPolygon $ diamond 40 40)
    dia2 = picColour darkSeaGreen  $ (picPolygon $ diamond 30 30)

demo3 :: IO ()
demo3 = writePS "basic3.ps" (psDraw $ dots `place` (P2 50 300) )
  where
    dots = dotX </> dotPlus </> dotAsterisk </> dotSquare </> dotPentagon
                </> dotTriangle </> dotDiamond

demo4 :: IO ()
demo4 = writePS "basic4.ps" (psDraw2 $ squares)
  where
    squares = picPolygon2 (square 10 zeroPt) 
         <++> picPolygon2 (square 20 zeroPt)
         <++> picPolygon2 (square 30 zeroPt)
         <++> picPolygon2 (square 40 zeroPt)
         <++> picPolygon2 (square 50 zeroPt)