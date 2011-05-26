{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZMidi.Emit.Utils.HList
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

module ZMidi.Emit.Utils.HList
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
  , concatH 

  ) where


import Data.Monoid

newtype H a = H { getH :: [a] -> [a] }

instance Monoid (H a) where
  mempty  = emptyH
  mappend = appendH

emptyH :: H a
emptyH = H $ id

wrapH :: a -> H a
wrapH a = consH a emptyH  

consH :: a -> H a -> H a
consH a f = H $ (a:) . getH f

snocH :: H a -> a -> H a
snocH  f a = H $ getH f . (a:)

appendH :: H a -> H a -> H a
appendH f g = H $ getH f . getH g

-- | Adding two things at once (note_on, note_off) is common in 
-- MIDI so there is a special case for it.
--
twoH :: a -> a -> H a
twoH a b = H $ \ls -> a:b:ls

toListH :: H a -> [a]
toListH = ($ []) . getH 

fromListH :: [a] -> H a
fromListH xs = H $ (xs++)

concatH :: [H a] -> H a
concatH = foldr appendH emptyH

