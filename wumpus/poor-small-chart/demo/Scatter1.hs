{-# OPTIONS -Wall #-}


-- See:
-- http://en.wikipedia.org/wiki/Scatter_plot

module Scatter1 where

import Wumpus.PSC.Axis
import Wumpus.PSC.Core  hiding ( ScaleCtx, Projection )
-- import Wumpus.PSC.DrawingUtils
-- import Wumpus.PSC.Legend
import Wumpus.PSC.ScaleMonad
import Wumpus.PSC.ScatterPlot


import Wumpus.Core                      -- package: wupus-core
import Wumpus.Basic.Graphic             -- package: wumpus-basic
import Wumpus.Basic.SafeFonts
import Wumpus.Basic.SVGColours

import Data.Maybe
import System.Directory


 


main :: IO ()
main = createDirectoryIfMissing True "./out/"
    >> writeChartEPS "./out/scatter_plot1.eps" pic1
    >> writeChartSVG "./out/scatter_plot1.svg" pic1


pic1 :: Picture Double
pic1 = fromMaybe errK $ drawGraphic $ 
          plot_layers . xaxis_graphic . yaxis_graphic

errK :: a
errK = error "Empty Graphic"

plot_layers :: DGraphic
plot_layers = runCoordScale scale_ctx $ plotLayers [layer1]


xaxis_graphic :: DGraphic
xaxis_graphic = axisMarks tickfun
                    $ runCoordScale scale_ctx 
                    $ xAxis 0 0 (+5) output_rect
  where
    tickfun = tickdown_textdownH 4 10 cfg
    cfg     = tickLabelConfig black (helvetica 12) ifloor



yaxis_graphic :: DGraphic
yaxis_graphic = axisMarks tickfun
                    $ runCoordScale scale_ctx 
                    $ yAxis 0 94 (+2) output_rect
  where
    tickfun = tickleftV 4 10 cfg
    cfg     = tickLabelConfig black (helvetica 12) ifloor


ifloor :: Double -> String
ifloor = step . floor 
  where
    step :: Int -> String
    step = show

{-
axis_fun :: ScaleCtx Double Double DGraphic
axis_fun = xa `cc` ya
  where
    xa = horizontalLabels (xAxisTickLabel tick_label_config ifloor)     x_axis_steps
    ya = verticalLabels   (yAxisTickLabel tick_label_config (ffloat 1)) y_axis_steps
    

tick_label_config :: TickLabelConfig
tick_label_config = TickLabelConfig black (helvetica 10) black 0.5 4 2


x_range         :: Range Double
x_range         = (-1.0) ::: 21.0

x_axis_steps    :: AxisSteps Double
x_axis_steps    = steps 0 (+5.0)

--
y_range         :: Range Double
y_range         = 92.5 ::: 103.5

y_axis_steps    :: AxisSteps Double
y_axis_steps    = steps 94.0 (+2.0)

-}



output_rect :: RectFrameLoc Double
output_rect = (zeroPt, RectFrame 450 400)



scale_ctx   :: ScaleCtx Double Double Double
scale_ctx = ScaleCtx xrange yrange
  where
    xrange = projectRange ((-1.0) ::: 21.0) (0.0 ::: 450.0) id
    yrange = projectRange  (92.5 ::: 103.5) (0.0 ::: 400.0) id


projectRange :: Fractional u 
             => Range ua -> Range u -> (ua -> u) -> Projection ua u
projectRange (ua0 ::: ua1) (u0 ::: u1) fromUA = 
   rescale (fromUA ua0) (fromUA ua1) u0 u1 . fromUA


layer1 :: ScatterPlotLayer Double Double Double
layer1 = (squareDot, input_data)

{-
drawingCtx :: Range Double -> Range Double -> DrawingContext Double Double
drawingCtx xr yr = drawingContext xr id yr id output_rect
-}


squareDot :: DotF Double
squareDot = filledRectangle green 10 10

steps :: u -> (u -> u) -> [u]
steps = flip iterate

input_data :: Dataset Double Double
input_data = zipWith (\x y -> (sz x, y)) [0..] response
  where
    sz    :: Int -> Double
    sz    = rescale 0 upper 0.0 20.0 . fromIntegral
    upper = fromIntegral $ (length response - 1)

response :: [Double]
response = [ 103.18, 101.68, 102.91, 101.28, 100.89, 100.36, 98.60, 99.07
           , 99.91, 98.62, 100.39, 96.88, 100.38, 97.53, 101.78, 98.63, 99.11
           , 98.62, 98.59, 99.06, 99.97, 99.09, 98.97, 98.69, 99.77, 98.79
           , 95.95, 97.52, 97.08, 96.59, 98.21, 98.69, 98.54, 97.74, 97.25
           , 97.65, 98.70, 99.77, 97.02, 94.92, 97.65, 96.46, 95.25, 95.39
           , 94.65, 93.15, 95.36, 95.05, 95.83, 94.44, 94.97, 96.85, 93.95
           , 93.36, 97.43, 94.09, 94.23, 97.51, 95.69, 94.35, 94.17]


