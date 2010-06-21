{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Neume.Extra.Common
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Common utils...
--
--------------------------------------------------------------------------------

module Neume.Extra.Common 
  ( 
    BarNum

  , strip

  ) where



type BarNum = Int



-- | strip - dual of const
--
strip :: a -> b -> b
strip _ b = b

