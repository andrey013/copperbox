{-# OPTIONS -Wall #-}

-- Recreating Figure 8. Laplace solver (top) from
-- the Repa paper...

module Plot1 where

import Wumpus.PSC.Axis
import Wumpus.PSC.Core
import Wumpus.PSC.ScaleRectMonad
import Wumpus.PSC.ScatterPlot


import Wumpus.Core                              -- package: wumpus-core

import Wumpus.Basic.Dots                        -- package: wumpus-basic
import Wumpus.Basic.Graphic
import Wumpus.Basic.Monads.CoordScaleMonad
import Wumpus.Basic.SafeFonts
import Wumpus.Basic.SVGColours


import System.Directory




main :: IO ()
main = createDirectoryIfMissing True "./out/"
    >> writeChartEPS "./out/laplace1.eps" pic1
    >> writeChartSVG "./out/laplace1.svg" pic1

pic1 :: Picture Double
pic1 = drawGraphicU $ scatter_plot


scatter_plot :: DGraphic
scatter_plot = runScaleRectM scale_ctx output_rect $ do 
    a <- plotLayers [(blueDot,input_data)]
    b <- xaxis_graphic
    c <- yaxis_graphic
    return (a . b . c)

xrange :: Range Int
xrange = 1 ::: 16

yrange :: Range Double
yrange = 0.0 ::: 7.0

xaxis_graphic :: ScaleRectM Int uy DGraphic
xaxis_graphic = drawXAxis tickfun (xAxisPoints OXBottom 1 (+1))
  where
    tickfun = tickdown_textdownH 4 10 cfg
    cfg     = tickLabelConfig black (helvetica 12) show



yaxis_graphic :: ScaleRectM ux Double DGraphic
yaxis_graphic = drawYAxis tickfun (yAxisPoints OYLeft 0 (+1.75))
  where
    tickfun = tickleftV 4 10 cfg
    cfg     = tickLabelConfig black (helvetica 12) (ffloat 2)


scale_ctx   :: DScaleCtx Int Double
scale_ctx = rectangleScaleCtx ( xrange, fromIntegral)
                              ( yrange, id)
                              (fst output_rect)


output_rect :: DRectangleLoc
output_rect = (Rectangle 450 300, zeroPt)


blueDot :: DotF
blueDot = dotCircle ( (standardAttr 18) { mark_colour = blue })


input_data :: Dataset Int Double
input_data = zip [1..] laplace

laplace :: [Double]
laplace = [ 1.00, 1.75, 2.50, 3.25, 3.75
          , 4.50, 5.00, 5.30, 5.50, 5.75
          , 6.00, 6.20, 6.40, 6.65, 6.80
          , 6.40 ]


