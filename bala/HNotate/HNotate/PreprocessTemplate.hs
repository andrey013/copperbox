
--------------------------------------------------------------------------------
-- |
-- Module      :  HNotate.PreprocessTemplate
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined
--
-- PreprocessTemplate - extract only the islands of interest from a .ly file.
-- (Abc to do)
--
--------------------------------------------------------------------------------

module HNotate.PreprocessTemplate where

import HNotate.ParserBase (waterMaybe)

import Control.Applicative hiding (many, optional, (<|>) )
import Control.Monad
import Text.ParserCombinators.Parsec


streamTokens :: [Token] -> String
streamTokens []     = ""
streamTokens (x:xs) = work x xs ""
  where
    work (Token x) []     d = d `append` x
    work (Token x) (y:ys) d = work y ys (d `append` x)
    
    append acc a = acc ++ " " ++ a

--------------------------------------------------------------------------------
-- Lexer


newtype Token = Token { getIsland :: String }
  deriving Show

charToken :: Char -> Token
charToken c = Token [c]
  
type Errors = [String]

type Lexer st = [Token] -> GenParser Char st [Token]

{-
getToken :: (Token -> Maybe a) -> TParser a
getToken test = token showf nextf testf
  where
    showf (_,tok) = show tok
    nextf (pos,_) = pos
    testf (_,tok) = test tok 
-}


lyLexerW :: GenParser Char st [Token] 
lyLexerW = step []
  where
    step cca  = (pfn cca) >>= maybe (return $ reverse cca) step
    pfn  cca  = waterMaybe (choiceA start_tokens cca)

start_tokens :: [Lexer st]    
start_tokens = [ lexLeftBrace, lexRightBrace, lexMetaComment, lexComment, 
                 lexKey, lexRelative, lexCadenza
                 ] 
 

choiceA :: [Lexer st] -> Lexer st
choiceA xs cca = foldr fn mzero xs
  where fn f g = f cca <|> g
    
trailSpace :: GenParser Char st Token -> Lexer st
trailSpace f cca = do 
    a   <- f
    -- parse trailing ws if there is any, don't collect...
    option '\0' (choice [space, eof >> return '\0']) 
    return $ a:cca

passBy :: GenParser Char st a -> Lexer st
passBy f cca = f >> return cca

next1 :: Lexer st -> Lexer st
next1 f = f >=> lexChunk

next2 :: Lexer st -> Lexer st
next2 f = f >=> lexChunk >=> lexChunk 


lexRelative :: Lexer st
lexRelative = next1 $ lexCommand "relative"

lexCadenza = choiceA [ lexCommand "cadenzaOn", lexCommand "cadenzaOff"]   

lexKey :: Lexer st
lexKey = next2 $ lexCommand "key"


     
lexCommand :: String -> Lexer st
lexCommand ss = try . trailSpace fn
  where fn = Token . ('\\':) <$> (char '\\' *> string ss) 
  
lexAnyCommand :: Lexer st
lexAnyCommand = try . trailSpace fn
  where fn = Token . ('\\':) <$> (char '\\' *> many letter)   

lexMetaComment :: Lexer st
lexMetaComment = try . trailSpace fn
  where 
    fn = (\ss -> Token $ "<<< " ++ ss ++ " >>>") <$> 
                (string "%{#" >> manyTill anyChar (try (string "#%}"))) 
    
lexComment :: Lexer st
lexComment = passBy $ 
    try $ string "%{" >> manyTill anyChar (try (string "%}")) 

lexLeftBrace :: Lexer st
lexLeftBrace = trailSpace $  
    charToken <$> char '{' 

lexRightBrace :: Lexer st
lexRightBrace = trailSpace $  
    charToken <$> char '}' 

lexChunk :: Lexer st
lexChunk = trailSpace $ 
    Token <$> many1 (alphaNum <|> oneOf "'_-")
  
  
        