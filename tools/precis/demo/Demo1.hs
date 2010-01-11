

module Demo where

import Precis.CabalData
import Precis.CabalExtract



import Text.ParserCombinators.Parsec
import Control.Applicative
import Control.Monad




test_file :: FilePath
test_file = "../samples/data-obscura.cabal"



demo1 = parseFromFile cabalInfo test_file


