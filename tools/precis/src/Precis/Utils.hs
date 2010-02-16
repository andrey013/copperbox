{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Precis.Utils
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Utils
--
--------------------------------------------------------------------------------


module Precis.Utils
  (
    para
  , mbCons
  , mbEither

  , predMaybe
  , predMaybeM

  ) where

-- paramorphism (generalizes catamorphism (fold))
para :: (a -> ([a], b) -> b) -> b -> [a] -> b
para _   b []     = b
para phi b (x:xs) = phi x (xs, para phi b xs)



mbCons :: Maybe a -> [a] -> [a]
mbCons oa xs = maybe xs (:xs) $ oa

mbEither :: b -> Maybe a -> Either b a
mbEither b = maybe (Left b) Right


predMaybe :: (a -> Bool) -> a -> Maybe a
predMaybe p a | p a       = Just a
              | otherwise = Nothing

predMaybeM :: Monad m => (a -> m Bool) -> a -> m (Maybe a)
predMaybeM mp a = mp a >>= \v -> if v then return (Just a) else return Nothing