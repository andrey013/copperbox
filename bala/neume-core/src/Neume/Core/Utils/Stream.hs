{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Neume.Core.Utils.Stream
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Stream - infinite list
--
--------------------------------------------------------------------------------


module Neume.Core.Utils.Stream
  ( 
  -- * Stream
    Stream(..)
  , repeat
  , merge
  , cycle
  , take

  ) where 

import Prelude ( foldr, Int, (<=), (-) )

--------------------------------------------------------------------------------


data Stream a = a ::: (Stream a)

infixr 5 :::

repeat :: a -> Stream a
repeat a = a ::: repeat a

merge :: Stream a -> Stream a -> Stream a
merge (a ::: sa) (b ::: sb) = a ::: b ::: merge sa sb

cycle :: [a] -> Stream a
cycle xs = foldr (:::) (cycle xs) xs


take :: Int -> Stream a -> [a]
take n _           | n <= 0 = []
take n (a ::: sa)           = a : take (n-1) sa