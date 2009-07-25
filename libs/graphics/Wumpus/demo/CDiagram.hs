

module CDiagram where

import Wumpus.Core.Geometric
import Wumpus.Core.Point
import Wumpus.Core.Pointwise
import Wumpus.Core.Polygon
import Wumpus.Core.Line
import Wumpus.Core.Transformations
import Wumpus.Core.Vector

import Wumpus.Drawing.Arrow
import Wumpus.Drawing.Basic
import Wumpus.Drawing.Label
import Wumpus.Drawing.PostScript


demo1 :: IO ()
demo1 = writePS "cdiagram1.ps" $ runWumpus st0 $ drawing1 where
  drawing1 = do { setupFont "Times-Roman" 15
                ; ps_translate 60 480 
                ; diagram1
                }
 

diagram1 :: WumpusM ()
diagram1 = mapM_ drawArr arrs >> mapM_ (\lbl -> fst $ getPicture lbl $ origin) labels
   where
    origin = zeroPt

drawArr = drawLine


arrs :: [DLineSegment2]
arrs = concat $ sequence [f1,f2,f3,f4] $ boundingBox $ square 100 zeroPt 
  where
    f1 = veeArrow . expandLine (0.8) . hline   100  . topLeft
    f2 = veeArrow . expandLine (0.8) . vline (-100) . topLeft
    f3 = veeArrow . expandLine (0.8) . hline   100  . bottomLeft
    f4 = veeArrow . expandLine (0.8) . vline (-100) . topRight


-- P B 
-- A C
labels :: [Picture]
labels = sequence [fP, fB, fA, fC] $ boundingBox $ square 100 zeroPt
  where
    fP = (picLabel "P" 10 10 `place`) . movexy . topLeft
    fB = (picLabel "B" 10 10 `place`) . movexy . topRight
    fA = (picLabel "A" 10 10 `place`) . movexy . bottomLeft
    fC = (picLabel "C" 10 10 `place`) . movexy . bottomRight

    movexy = pointwise (translate (-5) (-5))

