{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Language.GLSL.ParseMonad
-- Copyright   :  (c) Stephen Tetley 2012
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Parse Monad
-- 
--------------------------------------------------------------------------------




module Language.GLSL.ParseMonad  
  (

    Lexeme(..)
  , ParseT
  , ParseErr(..)
  , ParseState(..)
  , LexerState(..)
  , SrcPosn(..)
  , runParseT

  , TokenRep(..)
  , AlexInput
  , nextChar
  , nextPos
  , alexGetChar
  , alexInputPrevChar
  , setLexerState
  , getLexerState
  , getPosition
  , getCurrentLine
  , setLexerStateStartCode
  , usingInput
  , parseError
  , lexError  

  , lexemeMsg
 
  ) where

import Language.GLSL.Token

import Control.Applicative
import Control.Monad.Error
import Control.Monad.State


data Lexeme = L SrcPosn GlslToken
  deriving (Eq, Show)

instance TokenRep Lexeme where
  tokenRep (L _ tok) = show tok

       
newtype ParseT m a = ParseM { 
          getParseT :: ErrorT ParseErr (StateT ParseState m) a }
  deriving ( Functor, Monad, MonadError ParseErr, MonadState ParseState ) 

      
newtype ParseErr = ParseErr String
  deriving (Show) 
  
data ParseState = ParseState 
    { input_file      :: FilePath
    , lex_state       :: LexerState
    }  
  deriving (Show)
  
data LexerState = LexerState 
    { src_loc         :: !SrcPosn
    , prev_char       :: !Char
    , rest_of_input   :: String 
    , start_code      :: !Int
    , prev_tok        :: GlslToken
    }
  deriving (Show)
                    
  
data SrcPosn = SrcPosn 
    { spos_line       :: !Int
    , spos_col        :: !Int
    }
  deriving (Eq,Show)

instance (Functor m, Monad m) => Applicative (ParseT m) where
  pure = return
  (<*>) = ap


runParseT :: Monad m => ParseT m a
                   -> FilePath -> String 
                   -> m (Either ParseErr a)
runParseT m file_name contents =
  evalStateT (runErrorT $ getParseT m) (initialParseState file_name contents) 
 
  
  
initialParseState :: FilePath -> String -> ParseState
initialParseState path input = ParseState path lexstate 
  where
    lexstate = LexerState { src_loc         = SrcPosn 1 1
                          , prev_char       = '\n'
                          , rest_of_input   = input
                          , start_code      = 0 
                          , prev_tok        = Tk_EOF
                          }
    


class TokenRep tok where
  tokenRep :: tok -> String

instance TokenRep [Char] where
  tokenRep = id

-- The Alex API we need to implement
-- type AlexInput
-- alexGetChar       :: AlexInput -> Maybe (Char,AlexInput)
-- alexInputPrevChar :: AlexInput -> Char

type AlexInput = LexerState

nextChar :: LexerState -> Maybe (Char,LexerState)
nextChar (LexerState _   _ []     _  _)  = Nothing
nextChar (LexerState loc _ (s:ss) sc tk) = 
    Just $ (s, LexerState (nextPos s loc) s ss sc tk) 


nextPos :: Char -> SrcPosn -> SrcPosn
nextPos '\n' (SrcPosn l _) = SrcPosn (l+1) 1
nextPos '\t' (SrcPosn l c) = SrcPosn l (incr c) where
    incr i = (i + 8 - ((i-1) `mod` 8))
nextPos _    (SrcPosn l c) = SrcPosn l (c+1)



alexGetChar       :: AlexInput -> Maybe (Char,AlexInput)
alexGetChar = nextChar

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (LexerState _ pc _ _ _) = pc


setLexerState :: Monad m => LexerState -> ParseT m () 
setLexerState inp = 
    modify (\s -> s{lex_state = inp}) 
      

setsLexerState :: (Functor m, Monad m) 
               => (LexerState -> LexerState) -> ParseT m () 
setsLexerState fn = do
    s0 <- fmap lex_state get    
    modify (\s -> s{lex_state = fn s0}) 

  
getLexerState :: Monad m => ParseT m LexerState
getLexerState = gets lex_state


getPosition :: (Functor m, Monad m) => ParseT m SrcPosn
getPosition = src_loc <$> gets lex_state


getCurrentLine :: (Functor m, Monad m) => ParseT m String
getCurrentLine = (takeWhile notNl . rest_of_input) <$> gets lex_state 
  where
    notNl '\n' = False
    notNl _    = True

getPrevTok :: (Functor m, Monad m) => ParseT m GlslToken
getPrevTok = prev_tok <$> gets lex_state


setLexerStateStartCode :: (Monad m, Functor m) 
                       => Int -> ParseT m () 
setLexerStateStartCode code = 
    setsLexerState (\s -> s { start_code = code})
{-
    (LexerState pos pc rest _ tk) <- getLexerState     
    modify (\s -> s{lex_state = (LexerState pos pc rest code tk)}) 
-}


usingInput :: (Monad m, Functor m) 
           => (SrcPosn -> a -> Lexeme) 
           -> (String -> a) -> LexerState -> Int -> ParseT m Lexeme
usingInput f g (LexerState pos _ input _ _) len = do
    let ans@(L _ tk) = f pos (g str) 
    setsLexerState (\s -> s { prev_tok = tk})
    return ans
  where 
    str = take len input


instance Error ParseErr where
  noMsg     = ParseErr ""
  strMsg s  = ParseErr s  

parseError :: (Monad m, Functor m, TokenRep tok) => tok -> ParseT m a
parseError tok = reportError "parse error" tok

lexError :: (Functor m, Monad m, TokenRep tok) => tok -> ParseT m a
lexError tok = reportError "lex error" tok

reportError :: (Functor m, Monad m, TokenRep tok) => String -> tok -> ParseT m a
reportError s tok = do 
    pos  <- getPosition
    ln   <- getCurrentLine 
    prev <- getPrevTok
    throwError $ strMsg $ unlines [ s
                                  , srcPosnMsg pos
                                  , tok_msg
                                  , prev_msg prev
                                  , rest_msg ln ]
  where
    tok_msg  = "Current token: " ++ tokenRep tok
    prev_msg = \tk -> "Previous token: " ++ show tk 
    rest_msg = \ln -> "Rest of line: " ++ ln  


srcPosnMsg :: SrcPosn -> String
srcPosnMsg (SrcPosn l c) = "line: " ++ show l ++ ", col: " ++ show c

lexemeMsg :: Lexeme -> String
lexemeMsg (L posn tok) = srcPosnMsg posn ++ " " ++ show tok
