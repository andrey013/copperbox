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
  ( |*>),
  normalizeGroupedElements
  ) where

import Bala.Format.Score

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



normalizeGroupedElements :: Seq (ScGlyph pch dur) -> Seq (ScGlyph pch dur)
normalizeGroupedElements = F.foldl fn mempty 
  where
    fn se (ScGroup ScChord xs)        = se |> ScGroup ScChord (notes xs)
    fn se (ScGroup ScGraceNotes xs)   = se |> ScGroup ScGraceNotes (singles xs)
    fn se e                           = se |> e
    
    
    notes = filter isNote
    
    singles = filter (not . isGroup) 
     
isNote :: ScGlyph pch dur -> Bool
isNote (ScNote _ _) = True
isNote _            = False   

isGroup :: ScGlyph pch dur -> Bool
isGroup (ScGroup _ _) = True
isGroup _             = False

    