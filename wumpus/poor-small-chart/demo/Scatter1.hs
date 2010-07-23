{-# OPTIONS -Wall #-}


-- See:
-- http://en.wikipedia.org/wiki/Scatter_plot

module Scatter1 where

import Wumpus.PSC.Axis
-- import Wumpus.PSC.BasicAdditions
import Wumpus.PSC.Core
-- import Wumpus.PSC.DrawingUtils
import Wumpus.PSC.ScaleRectMonad
import Wumpus.PSC.ScatterPlot


import Wumpus.Core                              -- package: wupus-core
import Wumpus.Basic.Graphic                     -- package: wumpus-basic
import Wumpus.Basic.SafeFonts
import Wumpus.Basic.SVGColours

import Data.Maybe
import System.Directory



main :: IO ()
main = createDirectoryIfMissing True "./out/"
    >> writeChartEPS "./out/scatter_plot1.eps" pic1
    >> writeChartSVG "./out/scatter_plot1.svg" pic1


pic1 :: Picture Double
pic1 = fromMaybe errK $ drawGraphic $ scatter_plot

errK :: a
errK = error "Empty Graphic"


x_range :: Range Double
x_range = (-1.0) ::: 21.0
          
y_range :: Range Double
y_range = 92.5  ::: 103.5


scatter_plot :: DGraphic
scatter_plot = runScaleRectM (x_range,id) (y_range,id) output_rect $ do 
    a <- plotLayers [layer1]
    b <- xaxis_graphic
    c <- yaxis_graphic
    return (a . b . c)

xaxis_graphic :: ScaleRectM Double uy DGraphic
xaxis_graphic = drawXAxis tickfun (xAxisPoints OXBottom 5)
  where
    tickfun = tickdown_textdownH 4 10 cfg
    cfg     = tickLabelConfig black (helvetica 12) ifloor



yaxis_graphic :: ScaleRectM ux Double DGraphic
yaxis_graphic = drawYAxis tickfun (yAxisPoints OYLeft 2)
  where
    tickfun = tickleftV 4 10 cfg
    cfg     = tickLabelConfig black (helvetica 12) ifloor


border_graphic :: DGraphic
border_graphic = supply zeroPt $ border (black, LineWidth 1.0) $ fst output_rect

ifloor :: Double -> String
ifloor = step . floor 
  where
    step :: Int -> String
    step = show



output_rect :: RectangleLoc Double
output_rect = (Rectangle 450 400, zeroPt)



layer1 :: ScatterPlotLayer Double Double
layer1 = (squareDot, input_data)


squareDot :: DotF
squareDot = filledRectangle green 10 10


input_data :: Dataset Double Double
input_data = zipWith (\x y -> (sz x, y)) [0..] response
  where
    sz    :: Int -> Double
    sz    = rescale 0 upper 0.0 20.0 . fromIntegral
    upper = fromIntegral $ (length response - 1)

response :: [Double]
response = [ 103.18, 101.68, 102.91, 101.28, 100.89, 100.36, 98.60,  99.07
           , 99.91,  98.62,  100.39, 96.88,  100.38, 97.53,  101.78, 98.63,  99.11
           , 98.62,  98.59,  99.06,  99.97,  99.09,  98.97,  98.69,  99.77,  98.79
           , 95.95,  97.52,  97.08,  96.59,  98.21,  98.69,  98.54,  97.74,  97.25
           , 97.65,  98.70,  99.77,  97.02,  94.92,  97.65,  96.46,  95.25,  95.39
           , 94.65,  93.15,  95.36,  95.05,  95.83,  94.44,  94.97,  96.85,  93.95
           , 93.36,  97.43,  94.09,  94.23,  97.51,  95.69,  94.35,  94.17 ]


