{-# OPTIONS -Wall #-}

module Arrow1 where

import Wumpus.Core.Point
import Wumpus.Core.Wumpus
import Wumpus.Drawing.Arrow



demo1 :: IO ()
demo1 = writePS "arrow1.ps" $ runWumpus st0 $ drawing1 where
  drawing1 = do { translate 60 380 
                ; arrow (P2 0 0) (P2 40 40) }
             

demo0 :: IO ()
demo0 = writePS "arrow0.ps" $ runWumpus st0 $ drawing1 where
  drawing1 = do 
               translate 60 270
               newpath
               moveto 0 0 
               lineto 40 40
               vee 40 40 45

               moveto 100 100
               lineto 140 140
               triangle 140 140 45 
               closepath
               stroke

