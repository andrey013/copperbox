{-# LANGUAGE FlexibleContexts           #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Text.ParserCombinators.ZParse.ParseError
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- SrcPos - ...
--
--------------------------------------------------------------------------------

module Text.ParserCombinators.ZParse.ParseError
  ( 
    ParseFailure(..)
  , ErrMsg
  , ErrorStack
  , HasErrorStack(..)

  , failureMessage
  , putFailure
 
  ) where

import Text.ParserCombinators.ZParse.ParseMonad
import Text.ParserCombinators.ZParse.SourcePosition
import Text.ParserCombinators.ZParse.Utils

import Control.Applicative


newtype ParseFailure = ParseFailure { getParseFailure :: String }

instance Show ParseFailure where
  show = getParseFailure


newtype ErrMsg = ErrMsg { getErrMsg :: String }
  deriving (Eq,Show)

type ErrorStack = [ErrMsg]



class HasErrorStack st where
  getErrorStack :: st -> ErrorStack
  setErrorStack :: ErrorStack -> st -> st


failureMessage :: (HasSrcPos st, HasErrorStack st, 
                   HasInput st, Show (InputStream st)) 
               => st -> ParseFailure
failureMessage = liftA3 (ParseFailure `ooo` fn) getSrcPos getErrorStack getInput
  where
    fn pos estk inp =  "*** Parse Error: " ++ location pos ++ "\n" 
                    ++ errorStack estk
                    ++ "\n" ++ take 60 (show inp)


location :: SrcPos -> String
location = liftA2 (\l c -> "line " ++ show l ++ ", column " ++ show c) 
                         src_line
                         src_column

errorStack :: ErrorStack -> String
errorStack = unlines . map getErrMsg


putFailure :: ParseFailure -> ErrorStack -> ErrorStack
putFailure err stk = (ErrMsg $ getParseFailure err) :stk