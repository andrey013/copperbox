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

          writeChartEPS "./out/scatter1.eps" (pic `picBeside` legend)
          writeChartSVG "./out/scatter1.svg" (pic `picBeside` legend)

length_width_plot :: ScatterPlot Double Double
length_width_plot =  ScatterPlot scatter_scale output_rect 
                                               (Just grid_cfg)
                                               (Just axes_cfg)  Nothing
                       
x_range :: Range Double
x_range = 4.0 ::: 8.0

y_range :: Range Double
y_range = 1.8 ::: 4.8

output_rect :: DrawingRectangle
output_rect = drawing 200 200



scatter_scale :: XYProjection Double Double
scatter_scale = drawingProjection (x_range,id) (y_range,id) output_rect

sepal_cfg       :: DotF
sepal_cfg       = circledDot red    2.5

versicolor_cfg  :: DotF
versicolor_cfg  = circledDot green  2.5

virginica_cfg   :: DotF
virginica_cfg   = circledDot blue   2.5

slsw :: IrisData -> (Double,Double)
slsw iris = (sepal_length iris, sepal_width iris)




axes_cfg :: AxisLabelConfig Double Double
axes_cfg = AxisLabelConfig
      { x_axis_cfg      = Just (axis_x, xAxisText (black,helvetica12) 4 (ffloat 1))
      , y_axis_cfg      = Just (axis_y, yAxisText (black,helvetica12) 2 (ffloat 1))
      } 


type AxisF u = u -> Maybe (AxisLabelAlg u)

axisStep :: (u -> u) -> AxisF u
axisStep f = \u0 -> Just (AxisLabelAlg u0 f)


startingFrom :: u -> AxisF u -> AxisF u 
startingFrom u f = \_u0 -> f u 

gridConfig :: LineConfig 
           -> AxisF u
           -> AxisF v
           -> (Range u,Range v)
           -> GridConfig u v
gridConfig line_cfg fu fv (u0:::_, v0:::_) = GridConfig line_cfg (fu u0) (fv v0)

grid_cfg :: GridConfig Double Double
grid_cfg = gridConfig (LineConfig blue 0.5 Nothing) x_alg y_alg (x_range,y_range)
  where
    x_alg = axisStep (+1.0) `rap` startingFrom 4.5
    y_alg = axisStep (+0.5) `rap` startingFrom 2.0


axis_x :: AxisLabelAlg Double
axis_x = AxisLabelAlg
      { start_value     = 4.5
      , step_fun        = (+1.0)
      }

axis_y :: AxisLabelAlg Double
axis_y = AxisLabelAlg
      { start_value     = 2.0
      , step_fun        = (+0.5)
      }


legend :: DPicture
legend = drawLegend (LegendConfig (LabelConfig helvetica12 black) Nothing)
                    [ (red, "Sepal"), (green, "Versicolor"), 
                                      (blue,  "Viginica") ]


------------

axisAlg :: Range x  -> (x -> x) -> AxisLabelAlg x
axisAlg (u ::: _) f = AxisLabelAlg { start_value = u, step_fun = f }

drawing :: Double -> Double -> DrawingRectangle
drawing = DrawingRectangle


drawingProjection :: (Num u, Num v) 
                  => (Range u,u -> Double) 
                  -> (Range v,v -> Double) 
                  -> DrawingRectangle
                  -> XYProjection u v
drawingProjection (u@(u0:::_),fromU) (v@(v0:::_),fromV) (DrawingRectangle w h) =
    (xprojection, yprojection)
  where
    xprojection = Projection fromU (fromU u0) (w / fromU (rangeDist u))
    yprojection = Projection fromV (fromV v0) (h / fromV (rangeDist v))

