{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Precis.ParsecExtras
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Extra functions for Parsec... 
--
--------------------------------------------------------------------------------


module Precis.ParsecExtras
  (
     islandWater
   , stringci
   , withError  
  ) where

import Text.ParserCombinators.Parsec

import Control.Applicative hiding ( (<|>) )
import Control.Monad
import Data.Char



instance Applicative (GenParser tok st) where
  pure = return
  (<*>) = ap


islandWater :: GenParser tok st a 
            -> GenParser tok st b 
            -> GenParser tok st (Maybe a)
islandWater a b = liftM Just a <|> liftM (const Nothing) b


stringci :: String -> GenParser Char st String
stringci []     = return []
stringci (x:xs) = anyChar >>= \c ->
                  if x `equalCI` c then liftM (x:) (stringci xs)
                                   else unexpected [c]   
                  

equalCI :: Char -> Char -> Bool
equalCI a b | a == b    = True
            | otherwise = toUpper a == toUpper b


withError :: String -> GenParser tok st a -> GenParser tok st a
withError = flip (<?>)
