{-# LANGUAGE FlexibleContexts           #-}


module Demo1 where

import Graphics.Wumpus.Matrix
import Graphics.Wumpus.Wumpus

import Data.SG

import MonadLib


demo1 = runId $ runWriterT write6ret7


write6ret7 :: WriterM m String => m Int
write6ret7 = do 
  puts (7,"6")



demo2 :: String
demo2 = runWumpus st0 $ do
    writeln "ok"
    writeln "yes"

demo3 = putStr $ 
          runWumpus st0 $ moveto 10 10



demo4 = putStr $ runWumpus st0 $ arrows1 where
  arrows1 = withPage $ do 
              newpath