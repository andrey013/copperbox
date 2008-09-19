
-- ghci - 
-- :set -i..:../HNotate

module ParseAbcTest where

import HNotate.ParseAbc
import HNotate.ParserBase

import Text.ParserCombinators.Parsec



test_key_sig = parseTestState (commaSep keySig) tok_string 0 
  where
    tok_string = "A , Cmix, C , Am, Aminor "

test_modes = parseTestState (commaSep abcMode) tok_string 0
  where
    tok_string = "min , maj, mix, M "

l3 = parseTestState (commaSep ptemp) tok_string 0
  where ptemp = leading3 'm' 'i' 'x'
        tok_string = "mix, mixolydian, Mix "