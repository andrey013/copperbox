{-# OPTIONS -Wall #-}


-- See:
-- http://en.wikipedia.org/wiki/Iris_flower_data_set

module IrisMultiPlot where

import IrisParser

import Graphics.PSC.Axis
import Graphics.PSC.Core
import Graphics.PSC.DrawingUtils
import Graphics.PSC.ScatterPlot


import Wumpus.Core                      -- package: wumpus-core
import Wumpus.Extra.PictureLanguage
import Wumpus.Basic.Graphic             -- package: wumpus-basic
import Wumpus.Basic.SafeFonts
import Wumpus.Basic.SVGColours

import Data.List
import Data.Maybe
import System.Directory


 
main :: IO ()
main = do 
    { createDirectoryIfMissing True "./out/"
    ; ans <- readIrisData
    ; case ans of
        Nothing -> putStrLn "no go"
        Just infos -> sk infos
    }
  where
    sk infos = let pic = allPictures infos in do
        { writeChartEPS "./out/iris_multiplot.eps" (uniformScale 0.75 pic)
        ; writeChartSVG "./out/iris_multiplot.svg" pic 
        }


allPictures :: InfoSet -> DPicture
allPictures infos = 
    vspace 25 caption (verticalize [row1, row2, row3, row4])
  where
    box1  = labelBox "Sepal.Length"
  
    swsl  = makePic range_sepal_width  sepal_width 
                    range_sepal_length sepal_length
                    (Just sepal_width_x_axis) Nothing

    plsl  = makePic range_petal_length petal_length
                    range_sepal_length sepal_length
                    Nothing Nothing
  
    pwsl  = makePic range_petal_width  petal_width 
                    range_sepal_length sepal_length
                    (Just petal_width_x_axis) (Just sepal_length_y_axis)

    row1  = horizontalize [box1, swsl infos, plsl infos, pwsl infos]
    --------

    slsw  = makePic range_sepal_length sepal_length
                    range_sepal_width  sepal_width
                    Nothing (Just sepal_width_y_axis)

    box2  = labelBox "Sepal.Width"
   
    plsw  = makePic range_petal_length petal_length
                    range_sepal_width  sepal_width 
                    Nothing Nothing
  
    pwsw  = makePic range_petal_width  petal_width 
                    range_sepal_width  sepal_width 
                    Nothing Nothing

    row2  = horizontalize [slsw infos, box2, plsw infos, pwsw infos]
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
                    Nothing (Just petal_length_y_axis)

    row3  = horizontalize [slpl infos, swpl infos, box3, pwpl infos]
    --------

    slpw  = makePic range_sepal_length sepal_length
                    range_petal_width  petal_width 
                    (Just sepal_length_x_axis) (Just petal_width_y_axis)
   
    swpw  = makePic range_sepal_width  sepal_width
                    range_petal_width  petal_width
                    Nothing Nothing
                    
    plpw  = makePic range_petal_length petal_length
                    range_petal_width  petal_width
                    (Just petal_length_x_axis) Nothing
                    
    box4  = labelBox "Petal.Width"

    row4  = horizontalize [slpw infos, swpw infos, plpw infos, box4]



makePic :: Range Double -> (IrisData -> Double) 
        -> Range Double -> (IrisData -> Double) 
        -> Maybe (ScaleCtx Double Double DGraphic)
        -> Maybe (ScaleCtx Double Double DGraphic)
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
         -> ScaleCtx Double Double DGraphic
         -> ScatterPlot Double Double
makePlot x_range y_range axisF = 
    ScatterPlot (drawingCtx x_range y_range) (axisF `cc` border)
  where
    border = plainBorder black 0.5

tick_label_config :: TickLabelConfig
tick_label_config = TickLabelConfig black (helvetica 10) black 0.5 4 2

ifloor :: Double -> String
ifloor = step . floor 
  where
    step :: Int -> String
    step = show

sepal_width_x_axis :: ScaleCtx Double Double DGraphic
sepal_width_x_axis = horizontalLabelsTop labelF steps_sepal_width
  where
    labelF = xAxisTickLabelAlt tick_label_config (ffloat 1) 

sepal_width_y_axis :: ScaleCtx Double Double DGraphic
sepal_width_y_axis = verticalLabels labelF steps_sepal_width
  where
    labelF = yAxisTickLabel tick_label_config (ffloat 1) 

sepal_length_x_axis :: ScaleCtx Double Double DGraphic
sepal_length_x_axis = horizontalLabels labelF steps_sepal_length
  where
    labelF = xAxisTickLabel tick_label_config (ffloat 1) 
    
sepal_length_y_axis :: ScaleCtx Double Double DGraphic
sepal_length_y_axis = verticalLabelsRight labelF steps_sepal_length
  where
    labelF = yAxisTickLabelAlt tick_label_config (ffloat 1) 

petal_width_x_axis :: ScaleCtx Double Double DGraphic
petal_width_x_axis = horizontalLabelsTop labelF steps_petal_width
  where
    labelF = xAxisTickLabelAlt tick_label_config (ffloat 1) 

petal_width_y_axis :: ScaleCtx Double Double DGraphic
petal_width_y_axis = verticalLabels labelF steps_petal_width
  where
    labelF = yAxisTickLabel tick_label_config (ffloat 1) 

petal_length_x_axis  :: ScaleCtx Double Double DGraphic
petal_length_x_axis = horizontalLabels labelF steps_petal_length
  where
    labelF = xAxisTickLabel tick_label_config ifloor 
    
petal_length_y_axis  :: ScaleCtx Double Double DGraphic
petal_length_y_axis = verticalLabelsRight labelF steps_petal_length
  where
    labelF = yAxisTickLabelAlt tick_label_config ifloor 


range_sepal_length     :: Range Double
range_sepal_length     = 4.0 ::: 8.0

steps_sepal_length     :: AxisSteps Double
steps_sepal_length     = steps 4.5 (+1.0)

--
range_sepal_width      :: Range Double
range_sepal_width      = 1.8 ::: 4.8

steps_sepal_width      :: AxisSteps Double
steps_sepal_width      = steps 2.0 (+0.5)

--
range_petal_length     :: Range Double
range_petal_length     = 0.8 ::: 7.6

steps_petal_length     :: AxisSteps Double
steps_petal_length     = steps 1.0 (+1.0)

--
range_petal_width      :: Range Double
range_petal_width      = 0.0 ::: 2.6

steps_petal_width      :: AxisSteps Double
steps_petal_width      = steps 0.5 (+0.5)


output_rect :: DrawingRectangle
output_rect = (150,150)

drawingCtx :: Range Double -> Range Double -> DrawingContext Double Double
drawingCtx x_range y_range = drawingContext x_range id y_range id output_rect


dot_setosa      :: DotF
dot_setosa      = outlinedDot red    2

dot_versicolor  :: DotF
dot_versicolor  = outlinedDot green  2

dot_virginica   :: DotF
dot_virginica   = outlinedDot blue   2


extractData :: (IrisData -> Double, IrisData -> Double) -> IrisData -> (Double,Double)
extractData (fX,fY) = \iris -> (fX iris, fY iris)

labelBox :: String -> DPicture
labelBox title = fromMaybe errK $ drawGraphic $ rect . text
  where
    errK  = error "error - labelBox"
    rect  = strokedRectangle (black, LineWidth 0.5) w h (P2 0 0)
    text  = textlabelC (black,courier 18) title (P2 (w*0.5) (h*0.5))
    (w,h) = output_rect

steps :: u -> (u -> u) -> [u]
steps = flip iterate



horizontalize :: [DPicture] -> DPicture
horizontalize []     = error "horizontalize"
horizontalize (x:xs) = stackOnto (snd $ mapAccumL phi pic_displacement xs) x
  where
    phi h pic = (h + pic_displacement, moveH h pic) 


verticalize :: [DPicture] -> DPicture
verticalize []     = error "verticalize"
verticalize (x:xs) = 
    stackOnto (snd $ mapAccumL phi (negate pic_displacement) xs) x
  where
    phi v pic = (v - pic_displacement, moveV v pic) 

pic_displacement :: Double
pic_displacement = 1.20 * fst output_rect

caption :: DPicture
caption = frame $ textlabel (black, helvetica 18) msg (P2 120 0)
  where
    msg = "Iris Data (red=setosa, green=versicolor, blue=virginica)"
