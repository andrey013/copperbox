{-# LANGUAGE ScopedTypeVariables        #-}
{-# OPTIONS -Wall #-}


module IrisParser where


import Control.Monad

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as P


readIrisData :: IO (Maybe InfoSet)
readIrisData = 
    parseFromFile irisData "./data/iris.data" >>= \ ans -> 
    case ans of
       Left _ -> return Nothing
       Right a -> return $ Just a

--------------------------------------------------------------------------------
-- For the Iris data set...

type IrisClass = String

-- | (setosa , versicolor, virginica)
type InfoSet = ([IrisData],[IrisData],[IrisData])

data IrisData = IrisData 
      { sepal_length  :: Double
      , sepal_width   :: Double
      , petal_length  :: Double
      , petal_width   :: Double
      }
  deriving (Eq,Show)


irisData :: Parser InfoSet
irisData = liftM divideUp (many1 irisLine)

divideUp :: [(IrisClass,IrisData)] -> InfoSet
divideUp = foldr fn ([],[],[])
  where
    fn (name,r) (setosa,versicolor,virginica)
          | name == "Iris-setosa"     = (r:setosa, versicolor, virginica)
          | name == "Iris-versicolor" = (setosa, r:versicolor, virginica)
          | name == "Iris-virginica"  = (setosa, versicolor, r:virginica)
          | otherwise                 = (setosa, versicolor, virginica)


irisLine :: Parser (IrisClass,IrisData)
irisLine = do 
    sl   <- double1
    sw   <- double1
    pl   <- double1
    pw   <- double1
    name <- identifier
    return (name, IrisData sl sw pl pw)

double1 :: Parser Double
double1 = double >>= \ans -> comma >> return ans


--------------------------------------------------------------------------------
-- Lexer 

irisLex     :: P.TokenParser st
irisLex     = P.makeTokenParser $ 
                  emptyDef { identLetter = choice [letter, char '-'] }

double      :: Parser Double
double      = P.float irisLex 

comma       :: Parser String
comma       = P.comma irisLex

identifier  :: Parser String
identifier  = P.identifier irisLex

