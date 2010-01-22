{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Data.ParserCombinators.KangarooReader
-- Copyright   :  (c) Stephen Tetley 2009-2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Kangaroo parse monad with env.
--
--------------------------------------------------------------------------------

module Data.ParserCombinators.KangarooReader
  (
    module Data.ParserCombinators.Kangaroo.Combinators
  , module Data.ParserCombinators.Kangaroo.ParseMonad
  , module Data.ParserCombinators.Kangaroo.Prim
  , Kangaroo
  , parse
  , runKangaroo
  , ask

  ) where

import Data.ParserCombinators.Kangaroo.Combinators
import Data.ParserCombinators.Kangaroo.ParseMonad
import Data.ParserCombinators.Kangaroo.Prim

import Control.Monad ( liftM )

type Kangaroo e a = GenKangaroo e a


parse :: Kangaroo e a  
      -> e
      -> FilePath 
      -> IO (Either ParseErr a)
parse = runKangaroo 

runKangaroo :: Kangaroo e a
            -> e
            -> FilePath 
            -> IO (Either ParseErr a)
runKangaroo p env filename = liftM fst (runGenKangaroo p env filename)

ask :: Kangaroo e e
ask = getUserSt
