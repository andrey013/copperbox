--------------------------------------------------------------------------------
-- |
-- Module      :  Text.Diff.DiffKit.Parser 
-- Copyright   :  (c) Stephen Tetley 2007
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  portable (Parsec & Applicative)
--
-- Parse diffs

--------------------------------------------------------------------------------


module Text.Diff.DiffKit.Parser where

import Text.Diff.DiffKit.Datatypes


import Control.Applicative hiding (many, (<|>))
import Control.Monad
import Data.Char (isDigit)
import Data.List
import System.Time
import Text.ParserCombinators.Parsec hiding (token, space)

instance Applicative (GenParser tok st)  where
  pure = return
  (<*>) = ap

--------------------------------------------------------------------------------
-- The three toplevel functions
--------------------------------------------------------------------------------
parseContext :: Parser DiffFile
parseContext = DiffFile <$> cFileHeader <*> many cHunk
    
parseUnified :: Parser DiffFile
parseUnified = DiffFile <$> uFileHeader <*> many uHunk

parseNormal :: Parser DiffFile
parseNormal = DiffFile <$> return Nothing <*> many nHunk

--------------------------------------------------------------------------------
-- Context format parser
--------------------------------------------------------------------------------

cFileHeader = undefined

cHunk = undefined






--------------------------------------------------------------------------------
-- Unified format parser
--------------------------------------------------------------------------------

uFileHeader :: Parser (Maybe FileHeader)
uFileHeader =fn <$> headerFrom <*> headerTo
  where
     fn h1 h2 = Just (FileHeader h1 h2)
     
headerFrom = fileStats  (string "---")
headerTo   = fileStats  (string "+++")

fileStats :: Parser a -> Parser FileStats
fileStats start = FileStats <$> (token start             *> srcFileName) 
                            <*> token timeStamp   


uHunk :: Parser Hunk 
uHunk  = Hunk <$> uHunkHeader <*> many1 uLine


uHunkHeader :: Parser Header
uHunkHeader = UdHunkHeader <$> (dblAt   *> fromRange)
                           <*> (toRange <* dblAt)
                           <*  restOfLine





fromRange = token $ rangeP (char '-')
toRange   = token $ rangeP (char '+')
 
rangeP :: Parser a -> Parser (Int,Int)
rangeP startTok = (,) <$> (startTok  *> int) 
                      <*> (comma     *> int)
                      
                

uLine :: Parser HunkLine
uLine = choice [uAdd, uRemove, uCommon, incomplete]

uAdd     = Added   <$> (plus      *> restOfLine)
uRemove  = Removed <$> (dash      *> restOfLine)
uCommon  = Common  <$> (sglspace  *> restOfLine)

   
incomplete = Incomplete <$  string "\\ No newline at end of file"
                        <* newline










--------------------------------------------------------------------------------
-- Normal format parser
--------------------------------------------------------------------------------

nHunk = undefined nChangeCommand 

lineRange = sepBy1 int comma

nChangeCommand = undefined {- choice [nAdd,nReplace,nDelete] -}

nAdd     = (,,) <$> int       <*> char 'a' <*> lineRange
nReplace = (,,) <$> lineRange <*> char 'c' <*> lineRange
nDelete  = (,,) <$> lineRange <*> char 'd' <*> int


--------------------------------------------------------------------------------
-- date/time format
--------------------------------------------------------------------------------
timeStamp = unintrepretedTime

unintrepretedTime = StringTime <$> restOfLine

-- Helper datatypes
data Date = Date {
    year     :: Int,
    month    :: Int,
    day      :: Int
  }
  deriving Show 
  
data Time = Time {
    hour     :: Int,
    min      :: Int,
    sec      :: Int,
    sec_frac :: Integer
  }  
  deriving (Show)
  


  
isoTimeStamp :: Parser TimeStamp
isoTimeStamp = interpret    <$> token dashedDate
                            <*> token fractionedTime
                            <*> token timeZone
  where interpret _ _ _ = StringTime "err"                            

dashedDate :: Parser (Int,Month,Int)
dashedDate = (,,) <$> year <*> mon <*> day
  where 
    year = int4       <* dash
    mon  = int2Month  <* dash
    day  = int2

int2Month :: Parser Month
int2Month = build <$> int2   
  where build = toEnum . ((-) 1) 

fractionedTime = Time <$> fc <*> fc <*> fd <*> integer
  where
    fc      = int2 <* semi
    fd      = int2 <* dot

-- note read won't read a '+' prefixed int
timeZone :: Parser Int  
timeZone =  fn <$> choice [char '-', coercePlus] <*> count 4 digitchar 
  where
    coercePlus = '0' <$ plus
    
    fn :: Char -> [Char] -> Int
    fn a as = read $ (:) a as

--------------------------------------------------------------------------------
-- Tokens and useful combinators
--------------------------------------------------------------------------------

restOfLine =  manyTill (noneOf "\n") newline

dblAt = token (string "@@")

plus      = char '+'
semi      = char ':'
dot       = char '.'
dash      = char '-'
comma     = char ','
sglspace  = char ' ' 


-- handily filenames are terminated by a tab
-- so we can easily handle spaces in them
srcFileName :: Parser SourceFile
srcFileName = manyTill (noneOf "\t") tab

-- read a digit, returning it as a char
digitchar :: Parser Char
digitchar = satisfy isDigit

-- read a digit, returning it as an Int
digit :: Parser Int
digit = read . listify <$> digitchar

integer :: Parser Integer
integer = read <$> many1 digitchar 

int :: Parser Int
int = read <$> many1 digitchar 


listify a  = [a]


int2 :: Parser Int
int2 = intn 2
int4 = intn 4


intn :: Int -> Parser Int
intn n = read <$> count n digitchar

        
token :: Parser a -> Parser a
token p = p <* many sglspace



line :: Parser a -> Parser a           
line p  = p <* newline 



    