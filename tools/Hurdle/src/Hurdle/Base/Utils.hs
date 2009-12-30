{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Hurdle.Base.Utils
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- 
--
--------------------------------------------------------------------------------

module Hurdle.Base.Utils 
  (
    module Data.ParserCombinators.KangarooWriter
  , Parser
  , (<:>) 
  , applyfs
  , H
  , toList
  , logline
  , logPosition

  , stringTruncate
  ) where


import Data.ParserCombinators.KangarooWriter

import Control.Applicative
import Data.Monoid

type Parser a = Kangaroo (H Char) a    


infixr 5 <:>

-- | applicative cons
(<:>) :: Applicative f => f a -> f [a] -> f [a]
(<:>) p1 p2 = (:) <$> p1 <*> p2



-- applyfs is 'sequence' from Control.Monad but I don't want to drag in
-- Control.Monad.Instances
--
applyfs :: [(a -> b)] -> a -> [b]
applyfs []     _ = [] 
applyfs (f:fs) a = f a : applyfs fs a



-- Hughes list - same as DList but we don't want a dependency

newtype H a = H { unH :: [a] -> [a] }

fromList :: [a] -> H a
fromList xs = H (xs++)

toList :: H a -> [a]
toList f = unH f []

append :: H a -> H a -> H a
append f g = H $ unH f . unH g

charH :: Char -> H Char
charH = fromList . return 

stringH :: String -> H Char
stringH = fromList

instance Monoid (H a) where
  mempty = fromList []
  mappend = append

logline :: String -> Parser ()
logline s = tell $ stringH s `append` charH '\n'


logPosition :: String -> Parser ()
logPosition s = position >>= \pos -> 
    logline $ s ++ ", position " ++ show pos


--------------------------------------------------------------------------------

stringTruncate :: String -> String
stringTruncate = takeWhile (/= '\NUL')