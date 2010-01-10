

module FactExtract where

import Text.ParserCombinators.ZParse
import Control.Applicative
import Control.Monad

type Parser m a = CharParserT m a

parseFromFile :: Parser IO a -> FilePath -> IO a
parseFromFile p file_path = do 
   inp <- readFile file_path
   either (error . show) (return) =<< runCharParserT p (Just file_path) inp


test_file :: FilePath
test_file = "./samples/data-obscura.cabal"


-- What is a Cabal file?

-- prolog of fields (name: text)* where text may span multiple lines
-- Library section  (name: text)*
-- Executable section (name: text)*

-- The delimiters for fields are contain information - the 
-- next-field-name both terminates the last field and starts the 
-- new one.

-- Keywords e.g. "build-type:" can appear in the description, so 
-- it looks like the cabal parser is whitespace / indentation 
-- sensitive.



data CabalField = CabalField { cf_name :: String, cf_body :: String }
  deriving Show

parseField :: Monad m => String -> Parser m  CabalField
parseField name = CabalField <$> parsename <*> withinLine (many anyChar)
  where
    parsename = string name <* char ':'


name            :: Monad  m => Parser m CabalField 
name            = parseField "name"

version         :: Monad m => Parser m CabalField
version         = parseField "version"

stability       :: Monad m => Parser m CabalField
stability       = parseField "stability"



-- Note - factExtract stops on /parse-failure/ ...
-- 'stability' is not parsed because there are unmatched fields 
-- between 'version' and 'stability'.
--
{-
factExtract :: Parser [CabalField]
factExtract = runPerms $ mkList <$> atom name
                                <*> maybeAtom stability
                                <*> maybeAtom version
                                
  where 
    mkList a b c = a : (mbCons b (mbCons c []))
    mbCons Nothing  = id
    mbCons (Just x) = (x:)

-}

factExtract :: Monad m => Parser m [CabalField]
factExtract = (\a b -> [a,b]) <$> name <*> version

demo1 = parseFromFile factExtract test_file