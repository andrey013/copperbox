

module Demo where

import HMinCaml.Beta
import HMinCaml.Emit
import HMinCaml.Id
import HMinCaml.Parser
import HMinCaml.SparcAsm
import HMinCaml.Syntax
import HMinCaml.Type
import HMinCaml.Typing 



demo = do 
  ans <- parseMinCaml "ack.ml"
  case ans of 
    Left err -> putStrLn err
    Right a -> print a

-- aka dyap
demo02' = f 1 3 where
  f = (show .) . (+)
  


   