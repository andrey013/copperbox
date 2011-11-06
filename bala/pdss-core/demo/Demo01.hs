{-# OPTIONS -Wall #-}

module Demo01 where

import PDSS.Core.Monad

demo01 :: IO ()
demo01 = writeFile "demo01.pd" $ run (467,185,466,155) 12 $ do 
    text 94 44 "hello pd"
    b1 <- bang 20 20 
    return ()


