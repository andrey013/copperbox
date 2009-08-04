{-# OPTIONS -Wall #-}

module Label where

import Wumpus.Core.Line
import Wumpus.Core.Point

import Wumpus.Drawing.Basic
import Wumpus.Drawing.Label
import Wumpus.Drawing.X11Colours


demo1 :: IO ()
demo1 = writePicture "label1.ps" drawing1 where
  drawing1 = displace 60 380 $ withFont (timesRoman 15) $
                 someLine 
            <..> (picLabel "text1" 40 10)
            <..> displace 0 100 (withRgbColour seaGreen4 $ 
                                   withFont (timesRoman 5) $ labelGrid)
            <..> (displace 0 200 $ picText "line1\nline2" 40 20 10)

  someLine = withRgbColour seaGreen1 $ 
                      picLine $ LS (P2 0 0) (P2 50 40) 
       

labelGrid :: Picture
labelGrid = hcatSep 10 $ map (mkLabel . show . (\i -> (i,0::Int))) [1..10::Int]
  where
    mkLabel str = picLabel str 20 10


