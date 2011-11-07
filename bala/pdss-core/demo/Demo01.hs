{-# OPTIONS -Wall #-}

module Demo01 where

import PDSS.Core.Colour
import PDSS.Core.Monad

import Data.Int

demo01 :: IO ()
demo01 = writeFile "demo01.pd" $ run (467,185,466,155) 12 $ do 
    text 94 44 "hello pd"
    b1 <- bang 20 20 
    return ()




demo02 :: IO ()
demo02 = writeFile "demo02.pd" $ run (467,185,420,260) 12 $ do 
    canvas 10 10  30 20 black
    canvas 10 100 30 20 red
    return ()



toI32 :: Int -> Int32
toI32 = fromIntegral

