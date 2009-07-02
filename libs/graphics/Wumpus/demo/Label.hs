{-# OPTIONS -Wall #-}

module Label where

import Wumpus.Core.Fun
import Wumpus.Core.Line
import Wumpus.Core.Point

import Wumpus.Drawing.Basic
import Wumpus.Drawing.Label
import Wumpus.Drawing.PostScript
import Wumpus.Drawing.X11Colours


demo1 :: IO ()
demo1 = writePS "label1.ps" $ runWumpus st0 $ drawing1 where
  drawing1 = do { setupFont "Times-Roman" 15
                ; ps_translate 60 380 
                ; setRgbColour seaGreen1 
                ; drawLine $ LS (P2 0 0) (P2 50 40) 
                ; drawLabel $ label "text1" (P2 0 0)
                --
                ; ps_translate 0 100
                ; setRgbColour mistyRose4
                ; setupFont "Times-Roman" 10
                ; labelGrid
                }


labelGrid :: WumpusM ()
labelGrid = mapM_ drawLabel ls where
  ls = zipWith fn [1..10] (steps 30 300.0)
  fn i x = label (show (i,0)) (P2 x 0)