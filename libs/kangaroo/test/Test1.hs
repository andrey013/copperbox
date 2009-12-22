{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Test1
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Pickling
--
--------------------------------------------------------------------------------

module Test1 where

import qualified Picklers as P

import Data.ParserCombinators.Kangaroo


main :: IO ()
main = test01

test01 :: IO ()
test01 = do 
  P.writePickle "test01.bin" (P.cstring "Hello world")
  runKangaroo cstring "test01.bin" >>= print