{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Precis.Diff
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- 
--
--------------------------------------------------------------------------------


module Precis.Diff
  (
    Diff(..)

  , compareModules

  ) where

import Precis.Datatypes

import Data.Map
import qualified Data.Map as Map

data Diff a = InL a | InBoth a | InR a
  deriving (Eq,Ord,Show)

instance Functor Diff where
  fmap f (InL    a) = InL    (f a)
  fmap f (InBoth a) = InBoth (f a)
  fmap f (InR    a) = InR    (f a)


compareModules :: [SourceFile] -> [SourceFile] -> [Diff SourceFile]
compareModules xs ys = elems $ foldr insR `flip` ys $ foldr insL Map.empty xs
  where
    insL a s = insert a (InL a) s
    insR a s = insertWith (\_ _ -> InBoth a) a (InR a) s


-- Note Eq on Haskell-src-ext type will not work as Eq includes SrcLoc
