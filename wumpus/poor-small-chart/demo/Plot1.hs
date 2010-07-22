{-# OPTIONS -Wall #-}

-- Recreating Figure 8. Laplace solver (top) from
-- the Repa paper...

module Plot1 where

import Wumpus.PSC.Axis
import Wumpus.PSC.Core
import Wumpus.PSC.ScatterPlot


import Wumpus.Core                      -- package: wumpus-core
import Wumpus.Basic.Graphic             -- package: wumpus-basic
import Wumpus.Basic.SafeFonts
import Wumpus.Basic.SVGColours


import System.Directory


 


main :: IO ()
main = createDirectoryIfMissing True "./out/"
    >> writeChartEPS "./out/laplace1.eps" pic1
    >> writeChartSVG "./out/laplace1.svg" pic1


pic1 :: DPicture
pic1 = renderScatterPlot scatter_cfg [(blueDot,input_data)]
  where
   scatter_cfg = ScatterPlot (drawingCtx x_range y_range) (axis_fun `cc` border)
   border      = plainBorder black 0.5


axis_fun :: ScaleCtx Double Double DGraphic
axis_fun = xa `cc` ya
  where
    xa = horizontalLabels (xAxisTickLabel tick_label_config ifloor)     x_axis_steps
    ya = verticalLabels   (yAxisTickLabel tick_label_config (ffloat 2)) y_axis_steps
    

ifloor :: Double -> String
ifloor = step . floor 
  where
    step :: Int -> String
    step = show


tick_label_config :: TickLabelConfig
tick_label_config = TickLabelConfig black (helvetica 10) black 0.5 4 2


x_range         :: Range Double
x_range         = 1.0 ::: 16.0

x_axis_steps    :: AxisSteps Double
x_axis_steps    = steps 1.0 (+1.0)

--
y_range         :: Range Double
y_range         = 0.0 ::: 7.0

y_axis_steps    :: AxisSteps Double
y_axis_steps    = steps 0.0 (+1.75)


output_rect :: DrawingRectangle
output_rect = (450,300)

drawingCtx :: Range Double -> Range Double -> DrawingContext Double Double
drawingCtx xr yr = drawingContext xr id yr id output_rect

blueDot :: DotF
blueDot = outlinedDot white 4

steps :: u -> (u -> u) -> [u]
steps = flip iterate

input_data :: Dataset Double Double
input_data = zipWith (\x y -> (fromIntegral x, y)) [(1::Int)..] laplace

laplace :: [Double]
laplace = [ 1.00, 1.75, 2.50, 3.25, 3.75
          , 4.50, 5.00, 5.30, 5.50, 5.75
          , 6.00, 6.20, 6.40, 6.65, 6.80
          , 6.40 ]


