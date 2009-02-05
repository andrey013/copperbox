{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Language.GLSL.ParseMonad
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Parse Monad
-- 
--------------------------------------------------------------------------------




module Language.GLSL.ParseMonad  where

import Control.Applicative
import Control.Monad.Error
import Control.Monad.State
       
newtype ParseT m a = ParseM { 
          getParseT :: ErrorT ParseErr (StateT ParseState m) a }
  deriving ( Functor, Monad, MonadError ParseErr, MonadState ParseState ) 

      
newtype ParseErr = ParseErr String
  deriving (Show) 
  
data ParseState = ParseState {
      input_file      :: FilePath,
      lex_state       :: LexerState
    }  
  deriving (Show)
  
data LexerState = LexerState { 
      src_loc         :: !SrcPosn,
      prev_char       :: !Char,
      rest_of_input   :: String,
      start_code      :: !Int
    }
  deriving (Show)
                    
  
data SrcPosn = SrcPosn { 
      spos_line       :: !Int,
      spos_col        :: !Int
    }
  deriving (Eq,Show)

instance Monad m => Applicative (ParseT m) where
  pure = return
  (<*>) = ap


runParseT :: Monad m => ParseT m a
                   -> FilePath -> String 
                   -> m (Either ParseErr a)
runParseT m file_name contents =
  evalStateT (runErrorT $ getParseT m) (initialParseState file_name contents) 
 
  
  
initialParseState :: FilePath -> String -> ParseState
initialParseState path input = ParseState path lexstate where
    lexstate = LexerState { src_loc         = SrcPosn 1 1,
                            prev_char       = '\n',
                            rest_of_input   = input,
                            start_code      = 0 }
    



-- The Alex API we need to implement
-- type AlexInput
-- alexGetChar       :: AlexInput -> Maybe (Char,AlexInput)
-- alexInputPrevChar :: AlexInput -> Char

type AlexInput = LexerState

nextChar :: LexerState -> Maybe (Char,LexerState)
nextChar (LexerState _   _ []     _) = Nothing
nextChar (LexerState loc _ (s:ss) sc) = 
    Just $ (s, LexerState (nextPos s loc) s ss sc) 


nextPos :: Char -> SrcPosn -> SrcPosn
nextPos '\n' (SrcPosn l _) = SrcPosn (l+1) 1
nextPos '\t' (SrcPosn l c) = SrcPosn l (incr c) where
    incr i = (i + 8 - ((i-1) `mod` 8))
nextPos _    (SrcPosn l c) = SrcPosn l (c+1)


alexGetChar       :: AlexInput -> Maybe (Char,AlexInput)
alexGetChar = nextChar

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (LexerState _ pc _ _) = pc


setLexerState :: Monad m => LexerState -> ParseT m () 
setLexerState inp = 
    modify (\s -> s{lex_state = inp}) 
      

  
getLexerState :: Monad m => ParseT m LexerState
getLexerState = gets lex_state


getPosition :: Monad m => ParseT m SrcPosn
getPosition = src_loc <$> gets lex_state


getCurrentLine :: Monad m => ParseT m String
getCurrentLine = (takeWhile notNl . rest_of_input) <$> gets lex_state where
    notNl '\n' = False
    notNl _    = True



setLexerStateStartCode :: Monad m => Int -> ParseT m () 
setLexerStateStartCode code = do
    (LexerState pos pc rest _) <- getLexerState     
    modify (\s -> s{lex_state = (LexerState pos pc rest code)}) 









usingInput :: Monad m => 
    (SrcPosn -> a -> b) -> (String -> a) -> LexerState -> Int -> ParseT m b   
usingInput f g (LexerState pos _ input _) len = return $ f pos (g str) 
    where str = take len input


instance Error ParseErr where
  noMsg     = ParseErr ""
  strMsg s  = ParseErr s  

parseError :: Monad m => tok -> ParseT m a
parseError _ = reportError "parse error"

lexError :: Monad m => tok -> ParseT m a
lexError _ = reportError "lex error" 

reportError :: Monad m => String -> ParseT m a
reportError s = do 
    pos <- getPosition
    ln  <- getCurrentLine 
    throwError $ strMsg $ s ++ "\n" ++ show pos ++ "\n" ++ ln