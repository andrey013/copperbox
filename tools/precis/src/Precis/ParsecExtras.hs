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
     assertColumn1
   , manyUpto
   , islandWater

   , stringUpto
   , prefixStringUpto
   , prefixStringUptoci

   , charci
   , stringci

   , withError  
  ) where

import Text.ParserCombinators.Parsec

import Control.Monad
import Data.Char




-- | 'upto' captures the state, applies the parser, then restores
-- the state, effectively parsing without consuming anything.
--
manyUpto :: GenParser tok st a -> GenParser tok st b -> GenParser tok st [a]
manyUpto pa end = manyTill pa (try $ dontConsume end)



stringUpto :: GenParser Char st a -> GenParser Char st String
stringUpto delim = manyUpto anyChar delim


prefixStringUpto :: String -> GenParser Char st a -> GenParser Char st String
prefixStringUpto []     delim = stringUpto delim
prefixStringUpto (c:cs) delim = liftM2 (:) (char c) (prefixStringUpto cs delim)


prefixStringUptoci :: String -> GenParser Char st a -> GenParser Char st String
prefixStringUptoci []     delim = stringUpto delim
prefixStringUptoci (c:cs) delim = liftM2 (:) (charci c) 
                                             (prefixStringUptoci cs delim)


dontConsume :: GenParser tok st a -> GenParser tok st a
dontConsume p = do { pos <- getPosition
                   ; inp <- getInput
                   ; ans <- p
                   ; setPosition pos
                   ; setInput inp
                   ; return ans
                   }

assertColumn1 :: GenParser tok st a -> GenParser tok st a
assertColumn1 p = getPosition >>= checkCol >> p
  where
    checkCol pos | sourceColumn pos == 1 = return ()
                 | otherwise = fail "assertColumn1 - not at column 1" 

islandWater :: GenParser tok st a 
            -> GenParser tok st b 
            -> GenParser tok st (Maybe a)
islandWater a b = liftM Just a <|> liftM (const Nothing) b


stringci :: String -> GenParser Char st String
stringci []     = return []
stringci (x:xs) = liftM2 (:) (charci x) (stringci xs)
                  
charci :: Char -> GenParser Char st Char
charci x = anyChar >>= \c ->
    if x `equalCI` c then return c else unexpected [c]                      


withError :: String -> GenParser tok st a -> GenParser tok st a
withError = flip (<?>)



--------------------------------------------------------------------------------
-- helpers



equalCI :: Char -> Char -> Bool
equalCI a b | a == b    = True
            | otherwise = toUpper a == toUpper b


bracketM :: Monad m => m a -> (a -> m b) -> (a -> m c) -> m c
bracketM pre post mf = do 
    a   <- pre
    ans <- mf a
    _   <- post a
    return ans


