

module TestLexer where

import CTokens
import CLexerAlt

import System.IO


main = mapM_ runTest tests

tests = [ ("../parsertest/float_constants.txt", isCTokFLit)
        -- , ("../parsertest/string_constants.txt", isCTokSLit)
        ]

runTest (fname,pred) = do 
  text <- readFile fname
  let toks = takeWhile ((/=) CTokEof) $ tokens text
  case (all pred toks) of
    True -> putStrLn $ "Pass" ++ show toks
    False -> putStrLn "Fail" 


isCTokFLit :: CToken -> Bool
isCTokFLit (CTokFLit _)   = True
isCTokFLit _              = False

isCTokSLit :: CToken -> Bool
isCTokSLit (CTokSLit _)   = True
isCTokSLit _              = False




tokens :: String -> [CToken]
tokens str = case runAlex str (toks' []) of
              Left err -> error err
              Right xs -> xs
  where toks' acc = do { x <- get
                         ; case x of
                            CTokEof -> return (reverse acc) 
                            _ -> toks' (x:acc)}
                            

