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
y_range = 2.0 ::: 4.5

output_rect :: DrawingRectangle
output_rect = drawing 300 300 



scatter_scale :: XYProjection Double Double
scatter_scale = drawingProjection (x_range,id) (y_range,id) output_rect

sepal_cfg       :: DotConfig 
sepal_cfg       = DotConfig red 3

versicolor_cfg  :: DotConfig
versicolor_cfg  = DotConfig green 3

virginica_cfg   :: DotConfig
virginica_cfg   = DotConfig blue 3

slsw :: IrisData -> (Double,Double)
slsw iris = (sepal_length iris, sepal_width iris)




axes_cfg :: AxisLabelConfig Double Double
axes_cfg = AxisLabelConfig
      { axis_label_cfg  = LabelConfig helvetica12 black
      , x_axis_cfg      = Just (axis_x, ffloat 1)
      , y_axis_cfg      = Just (axis_y, ffloat 1)
      } 


gridConfig :: LineConfig 
           -> Maybe (u -> u) 
           -> Maybe (v -> v)
           -> (Range u,Range v)
           -> GridConfig u v
gridConfig line_cfg mbHf mbVf (u0:::_, v0:::_) = GridConfig line_cfg mbHalg mbValg
  where
    mbHalg = fmap (\fu -> AxisLabelAlg u0 fu) mbHf
    mbValg = fmap (\fv -> AxisLabelAlg v0 fv) mbVf


grid_cfg :: GridConfig Double Double
grid_cfg = GridConfig
      { grid_line       = LineConfig blue 0.5 Nothing
      , grid_x_axis     = Just axis_x
      , grid_y_axis     = Just axis_y
      } 

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

