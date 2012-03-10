{-# OPTIONS -Wall #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  DirectoryMetrics.WindowsParser
-- Copyright   :  (c) Stephen Peter Tetley 2012
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  experimental
-- Portability :  to be determined
--
-- Parser for output from Windows @dir@ command.
--
-----------------------------------------------------------------------------


module DirectoryMetrics.WindowsParser where


import DirectoryMetrics.Syntax
import DirectoryMetrics.ParserCombinators

import Control.Applicative
import Data.Char
import Data.Functor.Identity


toplevels :: Parser [Directory]
toplevels = dropLines 3 >> directories


directories :: Parser [Directory]
directories = many1 directory

directory :: Parser Directory
directory = Directory <$> 
    directoryLine <*> (dropLine *> many1 dirContent <* dropLines 2)



directoryLine :: Parser String
directoryLine = space >> reserved "Directory of" >> restOfLine



dirContent :: Parser DirContent
dirContent = 
    post <$> dateTime <*> eitherOf (reserved "<DIR>") csInteger <*> restOfLine
  where
    post dt (Left _)   rest = SubDir dt rest
    post dt (Right sz) rest = File dt sz rest
      

dateTime :: Parser DateTime
dateTime = lexeme $ 
    DateTime <$> (int <* char '/') <*> (int <* char '/') <*> lexeme int
             <*> (int <* char ':') <*> lexeme int


reserved :: String -> Parser ()
reserved ss = lexeme (string ss) >> return ()

csInteger :: Parser Integer
csInteger = post 0 <$> sepBy1 integer (char ',')
  where
    post ac []     = ac
    post ac (i:is) = post (ac * 1000 + i) is