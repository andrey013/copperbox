
module Language.C.Parser (
  -- * Parse functions
  parseTranslationUnit,
  parseTranslationUnitWithMode,
  -- * Parse mode
  ParseMode(..),
  Parser(..)
  ) where

{-
-- HAPPY
import Language.C.Happy.Parser
-}



-- FROWN:
import Language.C.Frown.Parser
import Language.C.Frown.Lexer 
import Language.C.Syntax 


data ParseMode = ParseMode {
  parseFilename :: String,
  parserType    :: Parser
  }
  deriving (Eq,Show)

-- | It should be possible to parameterize this module on which parser to use.
-- (Not currently implemented).
data Parser = Frown | Happy
  deriving (Eq,Show)

parseTranslationUnit :: String -> Either String CTranslationUnit
parseTranslationUnit text = frownParseTranslationUnit text ""

parseTranslationUnitWithMode :: String -> ParseMode -> Either String CTranslationUnit
parseTranslationUnitWithMode text mode@(ParseMode {parseFilename=filename}) 
  = case (parserType mode) of
      Frown -> frownParseTranslationUnit text filename
      Happy -> Left "Happy parser not yet integrated"

  
  