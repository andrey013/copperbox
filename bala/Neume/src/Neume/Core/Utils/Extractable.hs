{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Neume.Core.Utils.Extractable
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Project (sub-)values in and out of a base value.
--
--------------------------------------------------------------------------------


module Neume.Core.Utils.Extractable
  ( 
    Extractable(..)
  , into
  , outof
  , stmap_extr

  ) where 

import Neume.Core.Utils.StateMap

data Extractable a b = Extractable (a -> b) (b -> a -> a)

into :: Extractable a b -> b -> a -> a
into (Extractable _ upd) b a = upd b a

outof :: Extractable a b -> a -> b
outof (Extractable extr _) a = extr a


stmap_extr :: StateMap f 
           => Extractable st stx -> (stx -> a -> (b,stx)) -> st -> f a -> (f b,st)
stmap_extr extr f = stmap (\st a -> let (a',stx) = f (extract st) a
                                    in  (a',putback stx st)) 
  where
   extract  = outof extr
   putback  = into  extr
