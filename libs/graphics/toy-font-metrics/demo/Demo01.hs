{-# OPTIONS -Wall #-}



module Demo01 where


import Graphics.ToyFontMetrics.Parser

import Text.ParserCombinators.Parsec

import System.Directory


main :: IO ()
main = do 
    putStrLn "Nothing doing..."
    return ()


demo01 = parse versionNumber  "" "StartFontMetrics 3.0"