
module TestChord where

import Bala
import Text.ParserCombinators.Parsec hiding (token)




cmajor :: IO () 
cmajor = parseTest (many (token romanChord)) $ 
  "I ii iii IV V vi viio I"



        