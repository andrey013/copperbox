

module Demo where

-- import S1.CPSTrans
import S1.FreeVars
import S1.Parser
import S1.Syntax
import qualified S1.SyntaxCPS as CPS


import Data.Set
import Text.ParserCombinators.Parsec

demo1 = parseTest expr "fn x => x"
demo2 = parseTest expr "(fn x => x) (fn y => y)"
demo3 = parseTest expr "let g = fn x => x in g true end"
demo4 = parseTest expr "()"
demo5 = parseTest expr "1 + 3"
demo6 = parseTest expr "let g = fn x => x in g (1 + 3) end"


testFreeVars :: String  -> IO (Set Name)
testFreeVars src = do 
    let ans = parse expr "" src
    either fk sk ans
  where
    fk a = putStrLn (show a) >> return empty
    sk   = return . freeVariables  
    
demo_fv   = testFreeVars   "let g = fn x => x in g true end"
demo_fv'  = testFreeVars   "let g = fn x => x in g z end"


        
         

     