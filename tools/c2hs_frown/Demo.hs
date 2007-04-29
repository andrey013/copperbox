
-- :! ghc -c CParserFrown.hs
-- :load Demo.hs


module Main where

import CParserFrown
import CAST
import CLexerAlt
import CTokens
 
main = run header "int main(void) {int x; x=1+2;}"

demo = tokens "int main(void) {int x; x=1+2;}"

more xs = do 
  x <- get
  case x of
    CTokEof -> return (reverse xs) 
    _ -> more (x:xs)

tokens :: String -> IO [CToken]
tokens = run (more []) 
