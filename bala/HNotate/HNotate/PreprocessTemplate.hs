
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

import HNotate.ParserBase (Token(..), waterMaybe)

import Control.Applicative hiding (many, optional, (<|>) )
import Control.Monad
import Text.ParserCombinators.Parsec


type Preprocessor = GenParser Char NestCount [Token]
  

type PPStep st = [Token] -> GenParser Char st [Token]

type NestCount = Int


abcPrePro :: GenParser Char NestCount [Token] 
abcPrePro = step []
  where
    step   cca  = (pfn cca) >>= maybe (finish cca) step
    pfn    cca  = waterMaybe (choiceA abc_start_tokens cca)
    finish cca  = popCloseBrace cca >>= return . reverse
    
abc_start_tokens :: [PPStep NestCount]
abc_start_tokens = 
  [ lexNumFieldAbc, lexMeterFieldAbc, lexKeyFieldAbc, 
    
    lexMetaCommentAbc, lexCommentAbc
  ]

lexMetaCommentAbc :: PPStep NestCount
lexMetaCommentAbc = try . trailSpace fn
  where 
    fn = (\ss -> Token $ "%{#" ++ ss ++ " #%}") <$> 
                (string "%#" >> manyTill anyChar lineEnding) 
                
                
lexCommentAbc :: PPStep NestCount
lexCommentAbc = passBy $ 
    try $ string "%" >> manyTill anyChar lineEnding

-- Abc doesn't have nesting like LilyPond - but it makes further 
-- processing easier if we pretend it does.
 
lexNumFieldAbc :: PPStep NestCount
lexNumFieldAbc = popCloseBrace >=> abcField 'X'
-- lexNumFieldAbc = trailSpaceDrop (string "X:") >=> popCloseBrace >=> pushStartBrace

lexMeterFieldAbc :: PPStep NestCount
lexMeterFieldAbc = abcField 'M'

lexKeyFieldAbc :: PPStep NestCount
lexKeyFieldAbc = abcField 'K'

     
popCloseBrace :: PPStep NestCount
popCloseBrace cca = do 
    nc <- getState
    setState 0
    return $ replicate nc (Token "}") ++ cca

pushStartBrace :: PPStep NestCount      
pushStartBrace cca = do
    nc <- getState
    setState $ nc+1
    return $ (Token "{") : cca

lineEnding :: GenParser Char st ()
lineEnding = choice [ () <$ newline, eof]     

abcField :: Char -> PPStep NestCount
abcField c = trailSpace fn >=> pushStartBrace
  where 
    fn = Token . ([c,':'] ++) <$> 
                (string [c,':'] >> manyTill anyChar lineEnding) 
    
--------------------------------------------------------------------------------
-- LilyPond

lyPrePro :: GenParser Char st [Token] 
lyPrePro = step []
  where
    step cca  = (pfn cca) >>= maybe (return $ reverse cca) step
    pfn  cca  = waterMaybe (choiceA ly_start_tokens cca)

ly_start_tokens :: [PPStep st]    
ly_start_tokens = 
  [ lexLeftBrace, lexRightBrace, lexMetaCommentLy, lexCommentLy, 
    lexKeyLy, lexRelativeLy, lexCadenzaLy
  ] 




lexRelativeLy :: PPStep st
lexRelativeLy = next1 $ lexCommandLy "relative"

lexCadenzaLy = choiceA [ lexCommandLy "cadenzaOn", lexCommandLy "cadenzaOff"]   

lexKeyLy :: PPStep st
lexKeyLy = next2 $ lexCommandLy "key"


     
lexCommandLy :: String -> PPStep st
lexCommandLy ss = try . trailSpace fn
  where fn = Token . ('\\':) <$> (char '\\' *> string ss) 
  
lexAnyCommandLy :: PPStep st
lexAnyCommandLy = try . trailSpace fn
  where fn = Token . ('\\':) <$> (char '\\' *> many letter)   

lexMetaCommentLy :: PPStep st
lexMetaCommentLy = try . trailSpace fn
  where 
    fn = (\ss -> Token $ "%{#" ++ ss ++ "#%}") <$> 
                (string "%{#" >> manyTill anyChar (try (string "#%}"))) 
    
lexCommentLy :: PPStep st
lexCommentLy = passBy $ 
    try $ string "%{" >> manyTill anyChar (try (string "%}")) 

lexLeftBrace :: PPStep st
lexLeftBrace = trailSpace $  
    charToken <$> char '{' 

lexRightBrace :: PPStep st
lexRightBrace = trailSpace $  
    charToken <$> char '}' 

--------------------------------------------------------------------------------
-- Base parsers - useful for both LilyPond and Abc


lexChunk :: PPStep st
lexChunk = trailSpace $ 
    Token <$> many1 (alphaNum <|> oneOf "'_-")
  
choiceA :: [PPStep st] -> PPStep st
choiceA xs cca = foldr fn mzero xs
  where fn f g = f cca <|> g 
    
trailSpace :: GenParser Char st Token -> PPStep st
trailSpace f cca = do 
    a   <- f
    -- parse trailing ws if there is any, don't collect...
    option '\0' (choice [space, eof >> return '\0']) 
    return $ a:cca

trailSpaceDrop :: GenParser Char st a -> PPStep st
trailSpaceDrop f cca = do 
    a   <- f
    -- parse trailing ws if there is any, don't collect...
    option '\0' (choice [space, eof >> return '\0']) 
    return $ cca
    
passBy :: GenParser Char st a -> PPStep st
passBy f cca = f >> return cca

next1 :: PPStep st -> PPStep st
next1 f = f >=> lexChunk

next2 :: PPStep st -> PPStep st
next2 f = f >=> lexChunk >=> lexChunk 


charToken :: Char -> Token
charToken c = Token [c]
        