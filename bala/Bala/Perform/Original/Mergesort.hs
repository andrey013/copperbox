
-----------------------------------------------------------------------------
-- |
-- Module      :  Bala.Perform.Original.Mergesort
-- Copyright   :  (c) ??
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  --
-- Stability   :  stable
-- Portability :  portable
--
-- List mergesort on sequences.
--
-- Acknowledgement - Original by Ian Lynagh, see Data.List.
-- Minimally adapted for Data.Sequence.
--
-----------------------------------------------------------------------------

module Bala.Perform.Original.Mergesort (
  mergesort
  ) where

import Data.Foldable as F
import Data.Sequence



mergesort :: (a -> a -> Ordering) -> Seq a -> Seq a
mergesort cmp = mergesort' cmp . fmap wrap



mergesort' :: (a -> a -> Ordering) -> Seq (Seq a) -> Seq a
mergesort' cmp s = 
  case viewl s of
    EmptyL      -> empty
    (sx :< ssx) -> case viewl ssx of
                     EmptyL      -> sx
                     _           -> mergesort' cmp (merge_pairs cmp s)
                     

merge_pairs :: (a -> a -> Ordering) -> Seq (Seq a) -> Seq (Seq a)
merge_pairs cmp s = 
  case viewl s of
    EmptyL      -> empty
    (sx :< ssx) -> case viewl ssx of
                     EmptyL      -> s
                     (sy :< ssy) -> merge cmp sx sy <| merge_pairs cmp ssy
        


merge :: (a -> a -> Ordering) -> Seq a -> Seq a -> Seq a
merge cmp sa sb = 
    let a = viewl sa; b = viewl sb in 
    case (a,b) of
      (_, EmptyL)           -> sa
      (EmptyL,_)            -> sb
      (x :< sx, y :< sy)  -> case x `cmp` y of
                               GT -> y <| merge cmp (x <| sx) sy
                               _  -> x <| merge cmp sx        (y <| sy)
    
wrap :: a -> Seq a
wrap x = x <| empty


