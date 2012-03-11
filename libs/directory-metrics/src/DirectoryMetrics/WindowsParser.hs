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


module DirectoryMetrics.WindowsParser 
  ( 
    toplevelsFromFile
  , toplevels

  -- Debug...
  , dateTime

  ) where


import DirectoryMetrics.FlatSyntax
import DirectoryMetrics.ParserCombinators

import Control.Applicative

-- | Even though it is an Either, the parsing type supports 
-- streaming in IO. 
-- 
-- For instance supposing the listing file was corrupt after the 
-- second directory. If you only demand the results of the first 
-- directly the subsequent directories won\'t be parsed and no 
-- error will be signalled.
--
toplevelsFromFile :: FilePath -> IO (Either ParseError [Directory])
toplevelsFromFile path = parseFromFile path toplevels
    


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
    post dt (Left _)   rest = SD $ SubDir dt rest
    post dt (Right sz) rest = F  $ File dt sz rest
      

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