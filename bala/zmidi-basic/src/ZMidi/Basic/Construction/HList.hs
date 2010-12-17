{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZMidi.Basic.Construction.HList
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Hughes list
--
--------------------------------------------------------------------------------

module ZMidi.Basic.Construction.HList
  ( 

  -- * Hughes list
    H
  , emptyH
  , wrapH
  , consH
  , snocH
  , appendH
  , twoH

  , toListH
  , fromListH

  ) where


type H a = [a] -> [a]

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

-- | Adding two things at once (note_on, note_off) is common in 
-- MIDI so there is a special case for it.
--
twoH :: a -> a -> H a
twoH a b = \ls -> a:b:ls

toListH :: H a -> [a]
toListH = ($ [])

fromListH :: [a] -> H a
fromListH xs = (xs++)
