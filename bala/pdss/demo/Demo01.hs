{-# OPTIONS -Wall #-}

module Demo01 where

import Sound.PDSS.Monad

demo01 :: IO ()
demo01 = 
    writeFile "demo01.pd" $ run (467,185,466,155) 12 $ text_ 94 44 "hello pd"


