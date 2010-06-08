{-# OPTIONS -Wall #-}


-- See:
-- http://en.wikipedia.org/wiki/Iris_flower_data_set

module Scatter1 where

import IrisParser

import Graphics.PSC.Axis
import Graphics.PSC.Core
import Graphics.PSC.DrawingUtils
-- import Graphics.PSC.Legend
import Graphics.PSC.ScatterPlot


import Wumpus.Core
import Wumpus.Extra.PictureLanguage
import Wumpus.Extra.SafeFonts           -- package: wumpus-core
import Wumpus.Extra.SVGColours

import Data.Maybe
import System.Directory


 


main :: IO ()
main = createDirectoryIfMissing True "./out/"
    >> demo01

demo01 :: IO ()
demo01 = do 
    ans <- readIrisData
    case ans of
      Nothing -> putStrLn "no go"
      Just infos -> do 
          let pic = allPictures infos
          writeChartEPS "./out/scatter1.eps" pic
          writeChartSVG "./out/scatter1.svg" pic


allPictures :: InfoSet -> DPicture
allPictures infos = vsepA VCenter 20 row1 [row2, row3, row4]
  where
    box1  = labelBox "Sepal.Length"
  
    swsl  = makePic range_sepal_width  sepal_width 
                    range_sepal_length sepal_length
                    Nothing Nothing
   
    plsl  = makePic range_petal_length petal_length
                    range_sepal_length sepal_length
                    Nothing Nothing
  
    pwsl  = makePic range_petal_width  petal_width 
                    range_sepal_length sepal_length
                    Nothing Nothing

    row1  = hsepA HCenter 20 box1 [swsl infos, plsl infos, pwsl infos]
    --------

    slsw  = makePic range_sepal_length sepal_length
                    range_sepal_width  sepal_width
                    Nothing Nothing

    box2  = labelBox "Sepal.Width"
   
    plsw  = makePic range_petal_length petal_length
                    range_sepal_width  sepal_width 
                    Nothing Nothing
  
    pwsw  = makePic range_petal_width  petal_width 
                    range_sepal_width  sepal_width 
                    Nothing Nothing

    row2  = hsepA HCenter 20 (slsw infos) [box2, plsw infos, pwsw infos]
    --------

    slpl  = makePic range_sepal_length sepal_length
                    range_petal_length petal_length
                    Nothing Nothing
   
    swpl  = makePic range_sepal_width  sepal_width 
                    range_petal_length petal_length
                    Nothing Nothing
                    
    box3  = labelBox "Petal.Length"

    pwpl  = makePic range_petal_width  petal_width
                    range_petal_length petal_length
                    Nothing Nothing

    row3  = hsepA HCenter 20 (slpl infos) [swpl infos, box3, pwpl infos]
    --------

    slpw  = makePic range_sepal_length sepal_length
                    range_petal_width  petal_width 
                    Nothing Nothing
   
    swpw  = makePic range_sepal_width  sepal_width
                    range_petal_width  petal_width
                    Nothing Nothing
                    
    plpw  = makePic range_petal_length petal_length
                    range_petal_width  petal_width
                    Nothing Nothing
                    
    box4  = labelBox "Petal.Width"

    row4  = hsepA HCenter 20 (slpw infos) [swpw infos, plpw infos, box4]



makePic :: Range Double -> (IrisData -> Double) 
        -> Range Double -> (IrisData -> Double) 
        -> Maybe (ScaleCtx Double Double Graphic)
        -> Maybe (ScaleCtx Double Double Graphic)
        -> InfoSet
        -> DPicture
makePic xr xflt yr yflt mb_xaxis mb_yaxis infos = 
    renderScatterPlot (makePlot xr yr axisF) (makeLayers extr infos)
  where
    extr  = extractData (xflt,yflt) 
    
    axisF = case (mb_xaxis , mb_yaxis) of
              (Nothing, Nothing) -> const id
              (Just xF, Nothing) -> xF
              (Nothing, Just yF) -> yF
              (Just xF, Just yF) -> xF `cc` yF

makeLayers :: (IrisData -> (Double,Double)) 
           -> InfoSet 
           -> [(DotF, Dataset Double Double)]
makeLayers filt (setosa, versicolor, virginica) = 
    [ (dot_setosa,      map filt setosa)
    , (dot_versicolor,  map filt versicolor)
    , (dot_virginica,   map filt virginica)
    ]
           
                 
makePlot :: Range Double 
         -> Range Double 
         -> ScaleCtx Double Double Graphic
         -> ScatterPlot Double Double
makePlot x_range y_range axisF = 
    ScatterPlot (drawingCtx x_range y_range) (axisF `cc` border)
  where
    border = plainBorder black 0.5


      
range_sepal_length     :: Range Double
range_sepal_length     = 4.0 ::: 8.0

range_sepal_width      :: Range Double
range_sepal_width      = 1.8 ::: 4.8

range_petal_length     :: Range Double
range_petal_length     = 0.8 ::: 7.6

range_petal_width      :: Range Double
range_petal_width      = 0.0 ::: 2.6

output_rect :: DrawingRectangle
output_rect = (100,100)

drawingCtx :: Range Double -> Range Double -> DrawingContext Double Double
drawingCtx x_range y_range = drawingContext x_range id y_range id output_rect


dot_setosa      :: DotF
dot_setosa      = outlinedDot red    1.5

dot_versicolor  :: DotF
dot_versicolor  = outlinedDot green  1.5

dot_virginica   :: DotF
dot_virginica   = outlinedDot blue   1.5


extractData :: (IrisData -> Double, IrisData -> Double) -> IrisData -> (Double,Double)
extractData (fX,fY) = \iris -> (fX iris, fY iris)

labelBox :: String -> DPicture
labelBox title = fromMaybe errK $ drawGraphic $ rect . text
  where
    errK = error "error - labelBox"
    rect = strokedRectangle (black, LineWidth 0.5) 100 100 (P2 0 0)
    text = textlabelC (black,courier12) title (P2 50 50)

{-
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

-}
