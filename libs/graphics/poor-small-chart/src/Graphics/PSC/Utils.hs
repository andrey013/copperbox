{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.PSC.Utils
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC with TypeFamilies and more
--
-- Utilities
--
--------------------------------------------------------------------------------

module Graphics.PSC.Utils
  (
    H
  , emptyH
  , consH
  , snocH
  , appendH
  , toListH
  , fromListH

  ) where


type H a = [a] -> [a]

emptyH :: H a
emptyH = id

consH :: a -> H a -> H a
consH a f = (a:) . f

snocH :: H a -> a -> H a
snocH  f a = f . (a:)

appendH :: H a -> H a -> H a
appendH f g = f . g


toListH :: H a -> [a]
toListH = ($ [])

fromListH :: [a] -> H a
fromListH xs = (xs++)
