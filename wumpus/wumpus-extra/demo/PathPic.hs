{-# OPTIONS -Wall #-}


module PathPic where

import Wumpus.Core
import Wumpus.Extra.Path
import Wumpus.Extra.SVGColours

import System.Directory

-- Note 
-- Processing has (0,0) at top left, 
-- PostScript has (0,0) at bottom left
-- hence the @scale 1 (-1)@

main :: IO ()
main = do
    createDirectoryIfMissing True "./out/"
    writeEPS_latin1 "./out/Path1.eps"  pic1
    writeSVG_latin1 "./out/Path1.svg"  pic1



pic1 :: Picture Double
pic1 = frame $ ostroke () $ dog_kennel


-- Or, maybe a monadic formulation...

dog_kennel :: DPath
dog_kennel = pathFrom (0,0) $ 
                        line_to  (0,60)  >> line_to (40,100)
                     >> line_to  (80,60) >> line_to (80,0)
                     >> line_to  (60,0)  >> line_to (60,30)
                     >> curve_to (60,50) (50,60) (40,60)
                     >> curve_to (30,60) (20,50) (20,30)
                     >> line_to  (20,0)

