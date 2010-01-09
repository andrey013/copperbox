

module CharParser where

import Text.ParserCombinators.ZParse

import Control.Applicative


runSimple :: Monad m => CharParserT m a -> String -> m a
runSimple p input = do 
  ans <- runCharParserT p Nothing input
  either (error . show) (return . fst) ans


demo01 :: IO [Char]
demo01 = runSimple (return "tok1" <|> return "tok2") ""

demo02 :: IO [Char]
demo02 = runSimple (anyChar *> some anyChar) "abcde" where

demo03 :: IO Char
demo03 = runSimple (anyChar) "abcde"

demo04 :: IO [Char]
demo04 = runSimple (sepBy anyChar comma) "a,b,c,d,e" where
  comma = string ","


demo05 :: IO Char
demo05 = runSimple (oneOf "abc") "a,b,c,d,e"

-- error !
demo06 :: IO Char
demo06 = runSimple (noneOf "abc") "a,b,c,d,e"

-- dot in Parsec is part of ParsecToken (not ParsecChar like the 
-- other ones here).
--
-- Remove this definition when something like ParsecToken has 
-- been implemented.
--
dot :: CharParserT m Char
dot = satisfy (=='.')


ellipsis :: CharParserT m String
ellipsis = many1 dot

demo07 :: IO String
demo07 = runSimple (ellipsis) "... hello"

demo08 :: IO Char
demo08 = runSimple (dot) "... hello"

demo09 :: IO [Char]
demo09 = runSimple (manyTill (oneOf "abc") (oneOf "de")) "abcde"

demo10 :: IO String
demo10 = runSimple p "aaaaz..."
  where p = manyTill anyChar (char 'z')   
  -- not working anyChar and char 'z' are not mutallay exclusive