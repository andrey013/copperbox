
-- :! ghc -c CParserFrown.hs
-- :load Demo.hs


module Main where

import CParserFrown
import CAST
import CLexerAlt
import CTokens
import CPretty

import Text.PrettyPrint.HughesPJ
import System.Environment



main = do 
  args <- getArgs
  case args of 
    [a] -> parseAndPrint a
    _   -> putStrLn "Usage: Demo <filename>"
  where parseAndPrint :: String -> IO ()
        parseAndPrint fname = do text <- readFile fname
                                 case runLex text fname header of
                                    Left err -> do { putStr err
                                                   ; putStrLn ""
                                                   ; showToks text
                                                   }
                                    Right ans -> do { putStr $ render $ pretty ans
                                                    ; putStrLn "\nAST:"
                                                    ; putStr $ show ans }
        showToks :: String -> IO ()
        showToks text = let ans = tokens text
                        in case ans of 
                          Left err -> putStr $ "ERR: " ++ err
                          Right toks -> do { putStrLn "Tokens:" 
                                           ; mapM_ (putStrLn . show) toks }

                           

test01 = tokens "int main(void) {int x; x=1+2;}"
test02 = parse "static int x = 8;"

text_01 = "int main(void) {int x; x=1+2;}"


parse str = case runLex str "" header of
              Left err -> putStr err
              Right ans -> print ans

tokens s = runLex s "" toks              

toks :: Lexer [CToken]
toks = toks' []
  where toks' acc = do { x <- getToken
                         ; case x of
                            CTokEof -> return (reverse acc) 
                            _ -> toks' (x:acc)}
