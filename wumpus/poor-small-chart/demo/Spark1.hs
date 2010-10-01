{-# OPTIONS -Wall #-}

module Spark1 where

import Wumpus.PSC.Bivariate
import Wumpus.PSC.Core
import Wumpus.PSC.SparkLine

import Wumpus.Core                              -- package: wumpus-core
import Wumpus.Basic.Colour.SVGColours           -- package: wumpus-basic
import Wumpus.Basic.Graphic
import Wumpus.Basic.SafeFonts

import System.Directory


main :: IO ()
main =  do 
    createDirectoryIfMissing True "./out/"
    writeEPS_latin1 "./out/spark1.eps" chart1
    writeSVG_latin1 "./out/spark1.svg" chart1

chart1 :: DPicture
chart1 = liftToPictureU $ execDrawing ctx $ do
            draw $ runSparkline 10 spark_line `at` (P2 0 20)  
            draw $ textline "0123456789" `at` zeroPt
  where
    ctx = (secondaryColour aquamarine) $ standardContext 18


-- Sparklines could have a standard Univariate interpretation 
-- where each data point is a /char/ and Y takes the range from
-- the minimum and maximum in the data.

runSparkline :: (Fractional u, FromPtSize u) => Int -> SparkLine u u u -> LocGraphic u
runSparkline len spark = \pt ->
    sparklineScaling len (Range 0.0 1.0) id (Range 0.0 1.0) id >>= \sctx -> 
    let f = runBivariate (Range 0.0 1.0) (Range 0.0 1.0) sctx spark in f pt



-- This needs ranges from Bivariate, char-width and numeral-height 
-- from DrawingF, but don\'t want an amalgamation of Bivariate and
-- DrawingF so Ranges are parameters.

sparklineScaling :: (Num ux, Num uy, Fractional u, FromPtSize u) 
                 => Int -> Range ux -> (ux -> u) -> Range uy -> (uy -> u)
                 -> DrawingF (ScalingContext ux uy u)
sparklineScaling len rux fromUX ruy fromUY = 
    monoNumeralHeight    >>= \h -> 
    monoTextWidth    len >>= \w ->
    return $ rangeScalingContext rux (Range 0 w) fromUX ruy (Range 0 h) fromUY
   

spark_line :: SparkLine Double Double Double
spark_line = sparkLine spark_stroke data_points

spark_stroke :: SparkLineF Double
spark_stroke = simpleLine -- black 1.0


spark_rangeband :: BivLocGraphic Double Double Double
spark_rangeband = rangeBand 0.3 0.8 -- aquamarine


data_points :: Dataset Double Double
data_points =  univariateVerticals $  
    [ 0.95, 0.8, 0.3, 0.52, 0.62, 0.7, 0.5, 0.4, 0.25, 0.2 ]



{-
data_points :: Dataset Double Double
data_points = 
    [ (0.1, 0.95)
    , (0.2, 0.8) 
    , (0.3, 0.3)
    , (0.4, 0.52) 
    , (0.5, 0.62)
    , (0.6, 0.7)
    , (0.7, 0.5)
    , (0.8, 0.4)
    , (0.9, 0.25) 
    , (1,   0.2)
    ]
-}
