{-# OPTIONS -Wall #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.OptionOM
-- Copyright   :  (c) Stephen Peter Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  experimental
-- Portability :  to be determined
--
-- Option type representing one or many.
--
-----------------------------------------------------------------------------

module Data.OptionOM 
  ( 
     OptionOM

  ) where

data OptionOM a = One a | Many [a]
  deriving (Eq,Show)


instance Functor OptionOM where
  fmap f (One x)   = One (f x)
  fmap f (Many xs) = Many (fmap f xs) 
