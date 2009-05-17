{-# LANGUAGE FlexibleContexts           #-}


module Demo1 where

import qualified Graphics.Wumpus.Matrix as CTM
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



demo4 = writePS "square.ps" $ runWumpus st0 $ drawing1 where
  drawing1 = do 
               translate 20 20
               newpath
               moveto 0  0 
               lineto 0  70
               lineto 70 70
               lineto 70  0
               closepath
               fill
               stroke

writePS :: FilePath -> String -> IO ()
writePS filepath pstext = writeFile filepath (bang ++ pstext) 
  where
    bang = "%!PS-Adobe-2.0\n"