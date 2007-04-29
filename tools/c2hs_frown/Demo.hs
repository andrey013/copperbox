
-- :! ghc -c CParserFrown.hs
-- :load Demo.hs


module Main where

import CParserFrown
import CAST
import CLexerAlt
import CTokens
 
main = parse "int main(void) {int x; x=1+2;}"

demo = runAlex "int main(void) {int x; x=1+2;}" tokens

parse str = case runAlex str header of
              Left err -> putStr err
              Right ans -> print ans
              

tokens :: Alex [CToken]
tokens = tokens' []
  where tokens' acc = do { x <- get
                         ; case x of
                            CTokEof -> return (reverse acc) 
                            _ -> tokens' (x:acc)}
