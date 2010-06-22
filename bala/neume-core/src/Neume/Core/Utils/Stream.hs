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
  -- * Stream type
    Stream(..)

  -- * Operations
  , repeat
  , interleave
  , cycle
  , take
  , (<<)
  , trail

  ) where 

import Prelude ( Show(..), foldr, Int, (.), (<=), (-), showString, error )

--------------------------------------------------------------------------------


data Stream a = a ::: (Stream a)

infixr 5 :::

instance Show a => Show (Stream a) where
  showsPrec n s = showsPrec n (take 8 s) . showString " ..."

-- | Make a stream by repeating the supplied element.
--
repeat :: a -> Stream a
repeat a = a ::: repeat a

-- | Interleave two streams.
--
interleave :: Stream a -> Stream a -> Stream a
interleave (a ::: sa) (b ::: sb) = a ::: b ::: interleave sa sb

-- | Cycle a list to make a stream.
--
cycle :: [a] -> Stream a
cycle xs = foldr (:::) (cycle xs) xs


take :: Int -> Stream a -> [a]
take n _           | n <= 0 = []
take n (a ::: sa)           = a : take (n-1) sa


infixr 5 <<

-- | Prepend a list to a stream.
--
(<<) :: [a] -> Stream a -> Stream a
[]     << s = s
(x:xs) << s = x ::: (xs << s)


-- | Build a stream from a list. The last element is infinitely 
-- repeated to form the stream tail.
--
-- 'trail' throws a runtime error if supplied with an empty list.
--
trail :: [a] -> Stream a
trail []     = error "trail - empty list"
trail [x]    = repeat x
trail (x:xs) = x ::: trail xs

