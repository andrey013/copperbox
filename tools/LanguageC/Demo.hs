
-- :! ghc -c CParserFrown.hs
-- :load Demo.hs


module Main where

import Language.C.Parser
import Language.C.Syntax
import Language.C.Tokens
import Language.C.AG.Pretty

-- import Text.PrettyPrint.HughesPJ
import PPrint

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
                                    Right ans -> outputPP ans
                                                    
-- Output function if using PJ pretty printer
-- outputPJ tu = putStr $ render $ pretty tu  

-- Output function if using PPrint
outputPP ans = let doc = prettyCTranslationUnit ans
               in displayIO stdout (renderPretty 0.9 100 doc)

