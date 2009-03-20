

module Demo where

import HMinCaml.Main 
import HMinCaml.Parser



demo :: IO ()
demo = do 
  ans <- parseMinCaml "ack.ml"
  case ans of 
    Left err -> putStrLn err
    Right a -> print a




   