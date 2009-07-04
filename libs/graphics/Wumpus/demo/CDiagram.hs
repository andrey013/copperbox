

module CDiagram where

import Wumpus.Core.Geometric
import Wumpus.Core.Point
import Wumpus.Core.Pointwise
import Wumpus.Core.Polygon
import Wumpus.Core.Line
import Wumpus.Core.Transformations
import Wumpus.Core.Vector

import Wumpus.Drawing.Arrow
import qualified Wumpus.Drawing.Basic as B
import Wumpus.Drawing.Label
import Wumpus.Drawing.PostScript


demo1 :: IO ()
demo1 = writePS "cdiagram1.ps" $ runWumpus st0 $ drawing1 where
  drawing1 = do { setupFont "Times-Roman" 15
                ; ps_translate 60 480 
                ; diagram1
                }
 

diagram1 :: WumpusM ()
diagram1 = mapM_ drawArr arrs >> mapM_ drawLabel labels
 
drawArr = B.drawLine


arrs :: [DLineSegment2]
arrs = concat $ sequence [f1,f2,f3,f4] $ boundingBox $ square 100 zeroPt 
  where
    f1 = veeArrow . expandLine (0.8) . hline   100  . topLeft
    f2 = veeArrow . expandLine (0.8) . vline (-100) . topLeft
    f3 = veeArrow . expandLine (0.8) . hline   100  . bottomLeft
    f4 = veeArrow . expandLine (0.8) . vline (-100) . topRight


-- P B 
-- A C
labels ::[DLabel]
labels = sequence [fP, fB, fA, fC] $ boundingBox $ square 100 zeroPt
  where
    fP = label "P" . movexy . topLeft
    fB = label "B" . movexy . topRight
    fA = label "A" . movexy . bottomLeft
    fC = label "C" . movexy . bottomRight

    movexy = pointwise (translate (-4) (-4))