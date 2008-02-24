
module TestChord where

import Bala
import Text.ParserCombinators.Parsec hiding (token)




cmajor :: IO () 
cmajor = parseTest (many (token romanChord)) $ 
  "I ii iii IV V vi viio I"

triad01 = triad (read "I") ()
triad02 = triad (read "vi") ()

test01 = tip (read "I")

test02 = buildChord (read "C4") (IP [5,4])

test03 = buildChord (read "C4") (IP [4,5])

test04 = zack 10 [5,4]

test05 :: GuitarChord
test05 = read "G7"

test06 :: GuitarChord
test06 = read "A/Ab"        