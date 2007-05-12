
module Language.C.Parser (
  parseTranslationUnit,
  parseTranslationUnitWithMode )
  where

{-
-- HAPPY
import Language.C.Happy.Parser
-}



-- FROWN:
import Language.C.Frown.Parser
import Language.C.Frown.Lexer 
import Language.C.Syntax 


data ParseMode = ParseMode {
  parseFilename :: String
}

parseTranslationUnit :: String -> Either String CTranslationUnit
parseTranslationUnit text = frownParseTranslationUnit text ""


parseTranslationUnitWithMode = error "todo"
