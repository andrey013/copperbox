{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Neume.Core.Utils.HList
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Hughes list...
--
--------------------------------------------------------------------------------


module Neume.Core.Utils.HList
  ( 
  -- * Hughes list
    H
  , snoc

  ) where 


--------------------------------------------------------------------------------

type H a = [a] -> [a]

infixr 2 `snoc`
snoc :: H b -> b -> H b
snoc accf a = accf . (a:)
