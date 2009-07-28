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
demo1 = writePS "label1.ps" $ runWumpus env0 $ drawing1 where
  drawing1 = withFont (timesRoman 15) $  do 
                { ps_translate 60 380 
                ; withRgbColour seaGreen1 $ 
                      drawLine $ LS (P2 0 0) (P2 50 40) 
                ; fst $ getPicture (picLabel "text1" 40 10) $ (P2 0 0)
                --
                ; ps_translate 0 100
                ; withRgbColour seaGreen4 $ 
                      withFont (timesRoman 5) $ labelGrid
                }


labelGrid :: WumpusM ()
labelGrid = mapM_ (\lbl -> fst $ (getPicture lbl) zeroPt) ls where
  ls = zipWith fn [1..10] (steps 30 300.0)
  fn i x = picLabel (show (i,0)) 20 10 `place` (P2 x 0)