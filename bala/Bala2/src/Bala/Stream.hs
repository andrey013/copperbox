{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Stream
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Stream functions
--
--------------------------------------------------------------------------------

module Bala.Stream
  ( 
  -- * Extra stream functions
    unfoldSt 

  , paired
  , zap
  , headTail

  ) where


import Prelude hiding (head, tail, zipWith)


import Data.Stream ( Stream, head, tail, zipWith )
-- import qualified Data.Stream as S

import Data.Stream.Hinze.Stream ( (<:) )
-- import qualified Data.Stream.Hinze.Stream as HS


unfoldSt :: (a -> st -> Maybe (b,st)) -> st -> Stream a -> [b]
unfoldSt phi st strm = case phi (head strm) st of
  Just (a,st') -> a : unfoldSt phi st' (tail strm)
  Nothing      -> []



paired :: Stream a -> Stream (a,a) 
paired strm = (a,b) <: paired strm'' where
  (a,strm')  = (head strm, tail strm)
  (b,strm'') = (head strm', tail strm')


zap :: Stream (a->b) -> Stream a -> Stream b
zap = zipWith ($)

headTail :: Stream a -> (a,Stream a)
headTail s = (head s, tail s)