{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Neume.Core.Utils.HList
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Hughes list...
--
--------------------------------------------------------------------------------


module Neume.Core.Utils.HList
  ( 
  -- * Hughes list
    H
  , emptyH
  , wrapH
  , consH
  , snocH
  , appendH

  , toListH
  , fromListH

  ) where 


--------------------------------------------------------------------------------

type H a = [a] -> [a]

infixr 2 `snocH`


emptyH :: H a
emptyH = id

wrapH :: a -> H a
wrapH a = consH a id 

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
