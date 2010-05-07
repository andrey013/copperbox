{-# LANGUAGE ScopedTypeVariables        #-}
{-# OPTIONS -Wall #-}


module Scatter where

import Wumpus.Core
import Wumpus.Doodle.ScatterPlot
import Wumpus.Extra.SVGColours

import Control.Applicative ( liftA2 )
import Control.Monad

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as P


main :: IO ()
main = sequence_ [ test01 ]

-- This is wrong -- can't do one scatter plot over another as
-- each one is at liberty to change the /scaling/. This needs a 
-- rethink...
test01 :: IO ()
test01 = do 
    idata <- readIrisData

    let iris_virginica = mkIris "Iris-virginica" idata
    let plot1 = scatterPlot 40 26 blue iris_virginica

    let iris_versicolor = mkIris "Iris-versicolor" idata 
    let plot2 = scatterPlot 40 26 green iris_versicolor

    let iris_setosa = mkIris "Iris-setosa" idata 
    let plot3 = scatterPlot 40 26 maroon iris_setosa

    let pic = cropPlot $ plot1 `over` plot2 `over` plot3
    writeEPS_latin1 "./out/scatter01.eps" pic
    writeSVG_latin1 "./out/scatter01.svg" pic

  where
    mkIris name = map (liftA2 P2 sepal_width sepal_length) . filterByClass name

--------------------------------------------------------------------------------
-- For the Iris data set...


data IrisData = IrisData { 
        sepal_length  :: Double,
        sepal_width   :: Double,
        petal_length  :: Double,
        petal_width   :: Double,
        iris_class    :: String
      }
  deriving (Eq,Show)


filterByClass :: String -> [IrisData] -> [IrisData]
filterByClass name = filter ((==) name . iris_class)  

irisLex     :: P.TokenParser st
irisLex     = P.makeTokenParser $ 
                  emptyDef { identLetter = choice [letter, char '-'] }

double      :: Parser Double
double      = P.float irisLex 

comma       :: Parser String
comma       = P.comma irisLex

identifier  :: Parser String
identifier  = P.identifier irisLex

irisData :: Parser IrisData 
irisData = return IrisData `ap` dc `ap` dc `ap` dc `ap` dc `ap` identifier
  where dc = double >>= \d -> comma >> return d

readIrisData :: IO [IrisData]
readIrisData = 
    parseFromFile (many1 irisData) "./data/iris.data" >>= either fk return
  where
    fk = error . show    