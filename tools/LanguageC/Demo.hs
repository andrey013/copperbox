
-- :! ghc -c CParserFrown.hs
-- :load Demo.hs


module Main where

import Language.C.Parser
import Language.C.Syntax
import Language.C.Tokens
import Language.C.Pretty

import Text.PrettyPrint.HughesPJ
import System.Environment



main = do 
  args <- getArgs
  case args of 
    [a] -> parseAndPrint a
    _   -> putStrLn "Usage: Demo <filename>"
  where parseAndPrint :: String -> IO ()
        parseAndPrint fname = do text <- readFile fname
                                 case parseTranslationUnit text of
                                    Left err -> do { putStr err
                                                   }
                                    Right ans -> do { putStr $ render $ pretty ans
                                                    ; putStrLn "\nAST:"
                                                    ; putStr $ show ans }


