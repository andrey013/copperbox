

module Demo where

import HNanoML.Infer
import HNanoML.Parser
import HNanoML.Pretty
import HNanoML.Syntax
import HNanoML.Type
import HNanoML.Unification

import Text.PrettyPrint.Leijen  

-- maybe Syntax should be 'raw' and without type holes 
-- then gets filled in at type inference stage

demo :: IO ()
demo = do 
  ans <- parseNanoML "ack.ml"
  case ans of 
    Left err -> putStrLn err
    Right a -> putStrLn $ showWidth 80 (pretty $ infer a)
    
    

showWidth :: Int -> Doc -> String
showWidth w d = displayS (renderPretty 0.8 w d) ""


test01 = either print (print . infer) $ parseNanoML' "1 + 2"

test02 = either print (print . infer) $ parseNanoML' "print_int (1 + 2)"

test03 = either print (print . infer) $ parseNanoML' "print_int2 1"