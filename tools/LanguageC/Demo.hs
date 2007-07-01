
-- :! ghc -c CParserFrown.hs
-- :load Demo.hs


module Main where

import Language.C.Parser
import Language.C.Syntax
import Language.C.Tokens
import Language.C.Pretty

import Language.C.Pretty.EBPretty

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
                                                    ; outputPP ans }
                                 putStrLn $ "\n" ++ linesep
                                 putStrLn text
                                 
        linesep = replicate 80 '-'                                   
                                                  
-- Output function if using PJ pretty printer
-- outputPJ tu = putStr $ render $ pretty tu  

-- Output function if using PPrint
outputPP ans = let doc = pp ans
               in displayIO stdout (renderPretty 0.9 100 doc)

