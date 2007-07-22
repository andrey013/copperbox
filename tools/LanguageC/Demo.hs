
-- :! ghc -c CParserFrown.hs
-- :load Demo.hs


module Main where

import Language.C.Parser
import Language.C.Syntax
import Language.C.Tokens
import Language.C.Pretty

import Language.C.Pretty.MonadicEBP


import System.IO (stdout)
import System.Environment



main = do 
  args <- getArgs
  case args of 
    [a] -> parseAndPrint a
    _   -> putStrLn "Usage: Demo <filename>"
  where parseAndPrint :: String -> IO ()
        parseAndPrint fname = do text <- readFile fname
                                 case parseTranslationUnit text of
                                    Left err -> putStr err
                                    Right ans -> do { putStrLn "" -- (show ans)
                                                    ; putStrLn $ "\n" ++ linesep
                                                    ; outputEBP ans }
                                 putStrLn $ "\n" ++ linesep
                                 putStrLn text
                                 
        linesep = replicate 80 '-'                                   
                                                  


-- Output function if using PPrint
outputEBP ans = let doc = runPretty (pp ans) plainStyle
                in displayIO stdout (renderPretty 0.9 100 doc)

