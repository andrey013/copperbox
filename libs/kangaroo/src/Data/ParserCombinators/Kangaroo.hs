{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Data.ParserCombinators.Kangaroo
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Parser combinators with random access
--
--------------------------------------------------------------------------------

module Data.ParserCombinators.Kangaroo 
  (
    module Data.ParserCombinators.Kangaroo.ParseMonad
  , module Data.ParserCombinators.Kangaroo.Prim
  , Kangaroo
  , runKangaroo
  , parse
  ) where

import Data.ParserCombinators.Kangaroo.ParseMonad
import Data.ParserCombinators.Kangaroo.Prim



type Kangaroo a = GenKangaroo () a

runKangaroo :: Kangaroo a -> FilePath -> IO (Either ParseErr a)
runKangaroo p filename = runGenKangaroo p () filename >>= \(a,_) -> return a

-- jsut runKangaroo here
parse :: Kangaroo a -> FilePath -> IO (Either ParseErr a)
parse = runKangaroo 

