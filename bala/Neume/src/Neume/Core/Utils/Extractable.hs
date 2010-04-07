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
  , stmap_extr2

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

stmap_extr2 :: StateMap f 
            => Extractable st stx 
            -> Extractable st sty
            -> (stx -> a -> (r1,stx)) 
            -> (sty -> b -> (r2,sty))
            -> st 
            -> f (a,b) 
            -> (f (r1,r2),st)
stmap_extr2 extrA extrB f g = 
    stmap (\st (a,b) -> let (a',stx) = f (extractA st) a
                            st'      = putbackA stx st
                            (b',sty) = g (extractB st') b
                        in  ((a',b'), putbackB sty st'))
  where
   extractA = outof extrA
   putbackA = into  extrA
   extractB = outof extrB
   putbackB = into  extrB
