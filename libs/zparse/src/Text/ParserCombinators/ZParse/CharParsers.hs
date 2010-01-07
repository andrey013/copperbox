{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Text.ParserCombinators.ZParse.CharParsers
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- TokenParsers - ...
--
--------------------------------------------------------------------------------

module Text.ParserCombinators.ZParse.CharParsers where

-- import Text.ParserCombinators.ZParse.Combinators
-- import Text.ParserCombinators.ZParse.ParseMonad
import Text.ParserCombinators.ZParse.SourcePosition

data LexState = LexState {
                    ls_file_name :: Maybe String,
                    ls_src_pos   :: SrcPos,
                    ls_input     :: String
                  }
  deriving (Show)

  


