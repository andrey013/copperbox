{-# OPTIONS -Wall #-}



module Demo01 where


import Graphics.ToyFontMetrics.Parser
import Graphics.ToyFontMetrics.ParserCombinators


import System.Directory


main :: IO ()
main = do 
    putStrLn "Nothing doing..."
    return ()


demo01 = runParser integer  "120"
demo02 = runParser versionNumber "StartFontMetrics 3.0"