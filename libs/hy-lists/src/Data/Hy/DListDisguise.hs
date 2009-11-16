{-# OPTIONS -Wall #-}


--------------------------------------------------------------------------------
-- |
-- Module      :  Data.Hy.DListDisguise
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
-- 
-- Internal module - Data.DList disguised so the most useful 
-- functions for construction (e.g. show instances) can be easily 
-- imported.
--
--------------------------------------------------------------------------------

module Data.Hy.DListDisguise  
  (
    dlempty
  , dlout
  , dlwrap
  , (++++)
  , dlsnoc
  , dlcons
  ) where


import qualified Data.DList as DL


dlout :: DL.DList a -> [a]
dlout = DL.toList

dlempty :: DL.DList a 
dlempty = DL.empty

dlwrap :: a -> DL.DList a
dlwrap = DL.singleton

(++++) :: DL.DList a -> DL.DList a -> DL.DList a
(++++) = DL.append

dlsnoc :: DL.DList a -> a -> DL.DList a
dlsnoc = DL.snoc

dlcons :: a -> DL.DList a -> DL.DList a
dlcons = DL.cons
