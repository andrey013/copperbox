{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Text.ParserCombinators.ZParse.Utils
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Common utils
--
--------------------------------------------------------------------------------

module Text.ParserCombinators.ZParse.Utils 
  (
    subst
  , subst2

  ) where


-- aka (<*>), for some uses I prefer subst in the prefix form...

subst :: (a -> b -> c) -> (a -> b) -> a -> c
subst f g x = f x (g x)

subst2 :: (a -> b -> c -> d) -> (a -> b) -> (a -> c) -> a -> d
subst2 f g h x = f x (g x) (h x)

