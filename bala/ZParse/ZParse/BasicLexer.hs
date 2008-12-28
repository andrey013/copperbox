{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZParse.BasicLexer
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- BasicLexer - ...
--
--------------------------------------------------------------------------------

module ZParse.BasicLexer where

import ZParse.SrcPos
import ZParse.TokenParsers

import Data.Monoid

data BasicToken = BasicToken { _bt_tok :: String, _bt_loc :: SrcPos }

instance SourcePosition BasicToken where
  getSrcPos     (BasicToken _ l)  = l
  setSrcPos pos (BasicToken t _)  = BasicToken t pos

-- | mappend is interesting for Tokens (explain...)  
instance Monoid BasicToken where
  mempty        = BasicToken "" initialPos
  BasicToken l pos `mappend` BasicToken r _ = BasicToken (l++r) pos



 
   