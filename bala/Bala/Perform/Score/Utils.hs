--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Perform.Score.Utils
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Utility functions to work on Score rep.
--
--------------------------------------------------------------------------------

module Bala.Perform.Score.Utils (
  ( # ), ( #. ),
  ( |*>)
  ) where

import Bala.Perform.Score.Datatypes

import qualified Data.Foldable as F
import Data.Monoid
import Data.Sequence

-- | reverse apply
( # ) :: a -> (a -> b) -> b
a # f = f a

-- | reverse compose
( #. ) :: (a -> b) -> (b -> c) -> a -> c
f #. g = g . f



infixl 5 |*>
(|*>) se Nothing  = se
(|*>) se (Just e) = se |> e



     
isNote :: ScGlyph -> Bool
isNote (ScNote _ _) = True
isNote _            = False   

  