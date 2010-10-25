{-# OPTIONS -Wall #-}



module Demo01 where


import Graphics.ToyFontMetrics.Parser
import Graphics.ToyFontMetrics.ParserCombinators

import Control.Applicative
import System.Directory


main :: IO ()
main = do 
    putStrLn "Nothing doing..."
    return ()


demo01 = runParser versionNumber "StartFontMetrics 3.0\n"
demo02 = runParser newline  "\nComment ...\n"
demo03 = runParser (whiteSpace *> char 'z')  "Comment one-two-three\nz"
demo04 = runParser (manyTill anyChar newline) "one-two-three \n" 
demo05 = runParser (lexeme newline) "\n"
demo06 = runParser eof ""
demo07 = runParser eof "FAILURE"