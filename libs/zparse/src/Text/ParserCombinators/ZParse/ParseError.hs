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
 
  ) where

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


failureMessage :: (HasSrcPos st, HasErrorStack st) => st -> ParseFailure
failureMessage = liftA2 (ParseFailure `oo` fn) getSrcPos getErrorStack where
  fn pos estk = "*** Parse Error: " ++ location pos ++ "\n" ++ errorStack estk
                


location :: SrcPos -> String
location = liftA2 (\l c -> show l ++ ':' : show c) 
                         src_line
                         src_column

errorStack :: ErrorStack -> String
errorStack = unlines . map getErrMsg


