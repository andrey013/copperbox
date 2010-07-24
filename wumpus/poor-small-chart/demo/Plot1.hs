{-# OPTIONS -Wall #-}

-- Recreating Figure 8. Laplace solver (top) from
-- the Repa paper...

module Plot1 where

import Wumpus.PSC.Axis
import Wumpus.PSC.Bivariate
import Wumpus.PSC.Core
import Wumpus.PSC.ScatterPlot


import Wumpus.Core                              -- package: wumpus-core

import Wumpus.Basic.Dots                        -- package: wumpus-basic
import Wumpus.Basic.Graphic
import Wumpus.Basic.SafeFonts
import Wumpus.Basic.SVGColours


import System.Directory




main :: IO ()
main = createDirectoryIfMissing True "./out/"
    >> writeChartEPS "./out/laplace1.eps" pic1
    >> writeChartSVG "./out/laplace1.svg" pic1

pic1 :: Picture Double
pic1 = drawGraphicU $ scatter_plot . xaxis_graphic . yaxis_graphic


scatter_plot :: DGraphic
scatter_plot = plotLayers [(blueDot,input_data)] bv_config


xaxis_graphic :: DGraphic
xaxis_graphic = xAxisi OXBottom 1 tickfun bv_config 
  where
    tickfun = tickdown_textdownH 4 10 cfg
    cfg     = tickLabelConfig black (helvetica 12) show

yaxis_graphic :: DGraphic
yaxis_graphic = yAxis OYLeft 1.75 tickfun bv_config
  where
    tickfun = tickleftV 4 10 cfg
    cfg     = tickLabelConfig black (helvetica 12) (ffloat 2)



x_range :: Range Int
x_range = 1 ::: 16

y_range :: Range Double
y_range = 0.0 ::: 7.0

output_rect :: DRectangleLoc
output_rect = (Rectangle 450 300, zeroPt)

bv_config :: Bivariate Int Double
bv_config = bivariate (x_range,fromIntegral) (y_range,id) output_rect





blueDot :: DotF
blueDot = dotCircle ( (standardAttr 18) { mark_colour = blue })


input_data :: Dataset Int Double
input_data = zip [1..] laplace

laplace :: [Double]
laplace = [ 1.00, 1.75, 2.50, 3.25, 3.75
          , 4.50, 5.00, 5.30, 5.50, 5.75
          , 6.00, 6.20, 6.40, 6.65, 6.80
          , 6.40 ]


