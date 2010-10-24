{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.ToyFontMetrics.ParserCombinators
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- AFM file parser.
-- 
--------------------------------------------------------------------------------

module Graphics.ToyFontMetrics.ParserCombinators
  where

import Control.Applicative

data Result s r = Fail [s] | Okay r [s]

newtype Parser s r = Parser { getParser :: [s] -> Result s r }



instance Functor (Parser s) where
  fmap f mf = Parser $ \ss -> case getParser mf ss of
                                Okay a ss1 -> Okay (f a) ss1
                                Fail ss    -> Fail ss
     


instance Applicative (Parser s) where
  pure a    = Parser $ \ss -> Okay a ss
  mf <*> ma = Parser $ \ss -> case getParser mf ss of
                                Fail ss1 -> Fail ss1
                                Okay f ss1 -> case getParser ma ss of
                                  Fail _     -> Fail ss     -- backtrack both
                                  Okay a ss2 -> Okay (f a) ss2

instance Monad (Parser s) where
  return a = Parser $ \ss -> Okay a ss
  m >>= k  = Parser $ \ss -> case getParser m ss of
                               Fail ss1 -> Fail ss1
                               Okay a ss1 -> (getParser . k) a ss1
