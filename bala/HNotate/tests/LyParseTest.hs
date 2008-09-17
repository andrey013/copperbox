
-- ghci - 
-- :set -i..:../HNotate

module LyParseTest where

import HNotate.ParseLy
import HNotate.ParserBase

import Text.ParserCombinators.Parsec



test_durations = parseTestState (commaSep lyDuration) tok_string 0
  where
    tok_string = "4 , 4.. , \\breve , \\longa, 2*4 , 4*1/2"


    