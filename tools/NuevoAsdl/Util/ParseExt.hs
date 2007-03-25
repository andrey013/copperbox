
--  Helpers to parse command line args

module Util.ParseExt 
  ( defaultParser
  , maybeParser
  , readIntDefault
  , readIntMaybe
  )where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as P


defaultParser p d str = 
  case (parse p "" str) of
    Left err -> d
    Right a -> a 

maybeParser p str = 
  case (parse p "" str) of
    Left err -> Nothing
    Right a -> Just a 

integerParser = (integer >>= \x -> return $ fromIntegral x)    
    
readIntDefault :: Int -> String -> Int
readIntDefault d str = defaultParser integerParser d str

readIntMaybe :: String -> Maybe Int
readIntMaybe str = maybeParser integerParser str

emptyLex             = P.makeTokenParser emptyDef

integer           = P.integer emptyLex  

