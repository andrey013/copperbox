{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  GreyFold.Base.Crossings
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined
--
-- Zips and folds that cross between List and Sequence
-- (and sometimes Foldable).
--
--------------------------------------------------------------------------------


module GreyFold.Base.Crossings where

import qualified Data.Foldable as F
import Data.Sequence

-- Naming convention /prefix/@zip@/suffix/
-- prefix indicates the inputs, suffix the output, s - sequence, l - list 

slzips :: Seq a -> [b] -> Seq (a,b)
slzips = slzipsWith (,)

slzipsWith :: (a -> b -> c) -> Seq a -> [b] -> Seq c
slzipsWith f se ls = step (viewl se) ls where
  step (a :< sa) (b:bs)   = f a b <| step (viewl sa) bs
  step _         _        = empty

lsziplWith :: (a -> b -> c) -> [a] -> Seq b -> [c]
lsziplWith f ls se = step ls (viewl se) where
  step (a:as) (b :< sb)   = f a b : step as (viewl sb)
  step _      _           = []
  
genConcat :: F.Foldable c => 
    (z a -> z a -> z a) -> (z a) -> (t a -> z a) -> c (t a) -> z a
genConcat cat empt conv = F.foldr fn empt where
  fn a b = (conv a) `cat` b

smapl :: (a -> b) -> Seq a -> [b]
smapl f = F.foldr (\e a -> (f e) : a) []    

lmaps :: (a -> b) -> [a] -> Seq b
lmaps f = F.foldr (\e a -> (f e) <| a) empty 

-- The inner collection type is preserved so just use F.foldr
lsconcats :: [Seq a] -> Seq a
lsconcats = F.foldr (><) empty

-- The inner collection type is changed so use genConcat
lsconcatl :: [Seq a] -> [a]
lsconcatl = genConcat (++) [] (F.toList)
  