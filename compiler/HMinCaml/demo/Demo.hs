

module Demo where

import HMinCaml.Main 
import HMinCaml.Parser
import HMinCaml.Pretty
import HMinCaml.Typing

import Text.PrettyPrint.Leijen  

demo :: IO ()
demo = do 
  ans <- parseMinCaml "ack.ml"
  case ans of 
    Left err -> putStrLn err
    -- Right a   -> putStrLn (show a)
    Right a -> putStrLn $ showWidth 80 (pretty (typing a))
    
    

showWidth :: Int -> Doc -> String
showWidth w d = displayS (renderPretty 0.8 w d) ""




   