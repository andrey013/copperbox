

module Grid1 where

import Wumpus.Core.Frame
import Wumpus.Core.Instances
import Wumpus.Core.Point
import Wumpus.Core.PostScript
import Wumpus.Core.Transformations
import Wumpus.Core.Vector

import Wumpus.Drawing.Basic
import Wumpus.Drawing.Grid
import Wumpus.Drawing.X11Colours

--
import Wumpus.Test.TypeRestrict
import Wumpus.Core.VSExtra
import Wumpus.Core.Matrix


demo1 :: IO ()
demo1 = writePS "grid1.ps" $ runWumpus st0 $ drawing1 where
  drawing1 = do { ps_translate 60 380 
                ; setRgbColour dodgerBlue1 
                ; mapM_ drawLine $ grid (P2 0 0) (P2 5 4) 
                ---
                ; ps_translate 0 100
                ; setRgbColour dodgerBlue4
                ; mapM_ drawLine $ grid' (P2 0 0) (P2 5 4) frame1
                }
  frame1 = Frame2 (P2 0 0) (V2 1 0) (V2 0 0.5)


s01 :: Matrix3'3 Double 
s01 = elementarySwapRows 2 3 

s02 :: Matrix3'3 Double
s02 = elementaryReplace_i 2 6.0

s03 :: Matrix3'3 Double
s03 = elementaryReplace_i_j 3 1 2.0

d001 = m *# (P2 2 2) 
  where
    m = ftofD (ortho (P2 0 0)) (ortho (P2 1 1))


d002 = m *# (P2 2 2) 
  where
    m = ftofD (ortho (P2 1 1)) (ortho (P2 0 0)) 


d003 = m *# (P2 2 4) 
  where
    m = ftofD e f 
    e = Frame2 (P2 0 0) (V2 1 0) (V2 0 1)
    f = Frame2 (P2 4 2) (rotate (pi/4) (V2 1 0)) (rotate (pi/4) (V2 0 1))

d004 = P2 (4 - sqrt 2) (2 + 3*(sqrt 2))

d005 = d003 == d004


d006 :: (DVec2,DVec2)
d006 = let f = rotate (pi/4) in (f $ V2 1 0, f $ V2 0 1)