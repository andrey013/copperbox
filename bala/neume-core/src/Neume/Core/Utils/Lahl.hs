{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Neume.Core.Utils.Lahl
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Length annotated Hughes list...
--
--------------------------------------------------------------------------------


module Neume.Core.Utils.Lahl
  ( 
  -- * Hughes list
    Lahl
  , length
  , getH
  , empty
  , wrap
  , cons
  , snoc
  , append

  , toList
  , fromList

  ) where 

import Prelude ( id, (.), (+), (++), Int, ($) )
import qualified Prelude as Pre

--------------------------------------------------------------------------------

data Lahl a = Lahl { length :: Int, apply :: [a] -> [a] }


infixr 2 `snoc`


empty :: Lahl a
empty = Lahl 0 id

getH :: Lahl a -> ([a] -> [a])
getH = apply

wrap :: a -> Lahl a
wrap a = Lahl 1 (a:) 

cons :: a -> Lahl a -> Lahl a
cons a (Lahl n f) = Lahl (n+1)  ((a:) . f)

snoc :: Lahl a -> a -> Lahl a
snoc (Lahl n f) a = Lahl (n+1) (f . (a:))

append :: Lahl a -> Lahl a -> Lahl a
append f g = Lahl (length f + length g) (apply f . apply g)



toList :: Lahl a -> [a]
toList = ($ []) . apply

-- | NOTE - fromList is expensive as it needs to run length on 
-- the input list.
--
fromList :: [a] -> Lahl a
fromList [] = Lahl 0               id
fromList xs = Lahl (Pre.length xs) (xs++)
