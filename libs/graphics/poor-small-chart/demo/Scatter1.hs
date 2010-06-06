{-# LANGUAGE ScopedTypeVariables        #-}
{-# OPTIONS -Wall #-}


-- See:
-- http://en.wikipedia.org/wiki/Iris_flower_data_set

module Scatter1 where

import IrisParser

import Graphics.PSC.Axis
import Graphics.PSC.Core
import Graphics.PSC.Legend
import Graphics.PSC.ScatterPlot



import Wumpus.Extra.SafeFonts           -- package: wumpus-core
import Wumpus.Extra.SVGColours

import System.Directory

-- TEMP...
import Wumpus.Core
import Graphics.PSC.DrawingUtils
import Data.Maybe

infixr 5 `rap`
rap :: a -> (a -> b) -> b
rap a f = f a


main :: IO ()
main = createDirectoryIfMissing True "./out/"
    >> demo01

demo01 :: IO ()
demo01 = do 
    ans <- readIrisData
    case ans of
      Nothing -> putStrLn "no go"
      Just (setosa, versicolor, virginica) -> do 
          let pic =  renderScatterPlot length_width_plot $ 
                         [ (sepal_cfg,      map slsw setosa)
                         , (versicolor_cfg, map slsw versicolor)
                         , (virginica_cfg,  map slsw virginica)
                         ]

          writeChartEPS "./out/scatter1.eps" (addLegend pic)
          writeChartSVG "./out/scatter1.svg" (addLegend pic)
  where
   addLegend pic1 = pic1 `picBeside` (fromMaybe errK $ drawGraphic legend)
   errK           = error "Empty legend"

length_width_plot :: ScatterPlot Double Double
length_width_plot =  ScatterPlot scatter_scale output_rect 
                                               grid_cfg
                                               axes_cfg  Nothing
                       
x_range :: Range Double
x_range = 4.0 ::: 8.0

y_range :: Range Double
y_range = 1.8 ::: 4.8

output_rect :: DrawingRectangle
output_rect = drawing 200 200


scatter_scale :: XYProjection Double Double
scatter_scale = drawingProjection (x_range,id) (y_range,id) output_rect

sepal_cfg       :: DotF
sepal_cfg       = outlinedDot red    2.5

versicolor_cfg  :: DotF
versicolor_cfg  = outlinedDot green  2.5

virginica_cfg   :: DotF
virginica_cfg   = outlinedDot blue   2.5

slsw :: IrisData -> (Double,Double)
slsw iris = (sepal_length iris, sepal_width iris)




axes_cfg :: AxisF Double Double
axes_cfg = drawAxes (xAxisText (black,helvetica12) 4 (ffloat 1)) x_steps
                    (yAxisText (black,helvetica12) 2 (ffloat 1)) y_steps
      




grid_cfg :: GridF Double Double
grid_cfg = drawGrid drawF x_steps y_steps
  where
    drawF = simpleGridLine (blue, LineWidth 0.5)

x_steps :: AxisSteps Double
x_steps = iterate (+1.0) 4.5

y_steps :: AxisSteps Double
y_steps = iterate (+0.5) 2.0


legend :: Graphic
legend = drawLegend (simpleLegendElementDraw black helvetica12) 14
                    [ (red, "Sepal"), (green, "Versicolor"), 
                                      (blue,  "Viginica") ]


------------

drawing :: Double -> Double -> DrawingRectangle
drawing = (,)


drawingProjection :: (Num u, Num v) 
                  => (Range u,u -> Double) 
                  -> (Range v,v -> Double) 
                  -> DrawingRectangle
                  -> XYProjection u v
drawingProjection (u@(u0:::_),fromU) (v@(v0:::_),fromV) (w,h) =
    (xprojection, yprojection)
  where
    xprojection = Projection fromU (fromU u0) (w / fromU (rangeDist u))
    yprojection = Projection fromV (fromV v0) (h / fromV (rangeDist v))

