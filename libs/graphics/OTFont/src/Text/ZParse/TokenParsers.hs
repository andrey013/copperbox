{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Text.ZParse.TokenParsers
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- TokenParsers - ...
--
--------------------------------------------------------------------------------

module Text.ZParse.TokenParsers where

-- import Text.ZParse.Combinators
import Text.ZParse.ParseMonad

import Control.Applicative
--import Control.Monad.State
-- import Data.Monoid


-- @yield@ is similar to an /apomorphism/ the state yields a stream of 
-- (atomic) tokens until it is exhausted. At the end we may stiil be 
-- interested in the state, hence the type is /(Maybe ans,state)/ rather than
-- /Maybe (ans,state)/.
class Yield st token | st -> token where
  yield :: st -> (Maybe token,st)


-- @Token@ 
class TokenLiteral tok where tokenLiteral :: tok -> String


anyToken :: Yield st tok => ParserT st m tok
anyToken = ParserT $ \sk fk st -> f sk fk (yield st) where
    f _  fk (Nothing,st) = fk st
    f sk fk (Just t, st) = sk t fk st

tokenBy :: Yield st tok => (tok -> Bool) -> ParserT st m tok 
tokenBy apred = ParserT $ \sk fk st -> f sk fk (yield st) where
    f sk fk (Just t,st)  | apred t = sk t fk st
    f _  fk (_,     st)            = fk st


token :: (Yield st tok, TokenLiteral tok) => String -> ParserT st m tok
token sym = tokenBy (\t -> tokenLiteral t == sym)

token' :: (Yield st tok, Eq tok) => tok -> ParserT st m tok
token' tok = tokenBy (==tok)


oneOf :: (Yield st tok, TokenLiteral tok) => [String] -> ParserT st m tok
oneOf toks = tokenBy (\t -> (tokenLiteral t) `elem` toks)
    

noneOf :: (Yield st tok, TokenLiteral tok) => [String] -> ParserT st m tok
noneOf toks = tokenBy (\t -> not $ (tokenLiteral t) `elem` toks)


-- | Token parsers are somewhat different to Parsec   
-- Here token parsers generate some token type /tok/.

dot :: (Yield st tok, TokenLiteral tok) => ParserT st m tok
dot = token "." 


simpleWhite1 :: (Yield st tok, TokenLiteral tok) => ParserT st m tok
simpleWhite1 =  oneOf [" ","\n","\t"]


simpleWhiteSpace :: (Yield st tok, TokenLiteral tok) => ParserT st m [tok]
simpleWhiteSpace = many simpleWhite1

