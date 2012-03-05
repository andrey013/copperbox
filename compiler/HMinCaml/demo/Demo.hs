

module Demo where

import HMinCaml.HMonad
import qualified HMinCaml.KNormal as K
import HMinCaml.Parser
import qualified HMinCaml.Syntax as S
import HMinCaml.Typing
import qualified HMinCaml.Type as T


runFile :: FilePath -> IO ()
runFile file_path = do 
  ss <- readFile file_path
  putStrLn $ either id (show . K.ppExpr) $ compile ss

compile :: String -> Either String K.Expr
compile ss = parseMinCamlString ss >>= infer >>= K.knormal


tyCheck :: String -> Either String S.Expr
tyCheck ss = parseMinCamlString ss >>= infer


demo01 :: IO ()
demo01 = runFile "ack.ml"


runTString :: String -> IO ()
runTString ss = 
  putStrLn $ either id (show . K.ppExpr) $ compile ss


demo02 :: IO ()
demo02 = runTString "2 + 4"

demo03 :: IO ()
demo03 = runTString "let rec add1 x = x + 1 in add1 6"

demo03a :: IO ()
demo03a = 
  putStrLn $ either id (show . S.pptExpr) $ tyCheck "let rec add1 x = x + 1 in add1 6"


demo04 :: IO ()
demo04 = runTString $ unlines $ 
    [ "let rec add1 x = x + 1 in"
    , "let rec sub1 x = x - 1 in"
    , "print_int (add1 (sub1 4))"
    ]

demo04a :: IO ()
demo04a = runTString $ unlines $ 
    [ "let rec add1 x = x + 1 in"
    , "let rec sub1 x = x - 1 in"
    , "add1 (sub1 4)"
    ]

