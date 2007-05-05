
-- :! ghc -c CParserFrown.hs
-- :load Demo.hs


module Main where

import CParserFrown
import CAST
import CLexerAlt
import CTokens
 
main = parse "static int x = 8;" 

test01 = "int main(void) {int x; x=1+2;}"


x001 = parse "static int x = 8;"


demo = tokens "int main(void) {int x; x=1+2;}"

parse str = case runAlex str header of
              Left err -> putStr err
              Right ans -> print ans

tokens s = runAlex s toks              

toks :: Alex [CToken]
toks = toks' []
  where toks' acc = do { x <- get
                         ; case x of
                            CTokEof -> return (reverse acc) 
                            _ -> toks' (x:acc)}
