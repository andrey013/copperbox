{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Data.ParserCombinators.KangarooState
-- Copyright   :  (c) Stephen Tetley 2009, 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Kangaroo parse monad with user state.
--
--------------------------------------------------------------------------------

module Data.ParserCombinators.KangarooState
  (
    Kangaroo
  , parse
  , runKangaroo
  , evalKangaroo
  , execKangaroo
  , put
  , get
  , modify
  , gets
  -- Re-exports from ParseMonad
  -- * Parser types
  , ParseErr

  -- * Region types
  , RegionCoda(..)
  , RegionName    


  -- * Lift IO actions
  , liftIOAction

  -- * Error reporting and exception handling
  , reportError
  , substError

  -- * Primitive parsers
  , word8
  , satisfy
  , checkWord8
  , opt 
  , moveForward

  -- * Query the cursor position
  , position
  , region
  , atEnd
  , lengthRemaining
  , regionSize

  -- * Parse within a region
  , intraparse
  , advance
  , advanceRelative
  , restrict
  , restrictToPos
   
  -- * Debug
  , printHexAll
  , printRegionStack 

  , module Data.ParserCombinators.Kangaroo.Combinators
  , module Data.ParserCombinators.Kangaroo.Prim

  ) where

import Data.ParserCombinators.Kangaroo.Combinators
import Data.ParserCombinators.Kangaroo.ParseMonad
import Data.ParserCombinators.Kangaroo.Prim
import Data.ParserCombinators.Kangaroo.Utils

import Control.Monad

type Kangaroo st a = GenKangaroo st a

parse :: Kangaroo st a -> st -> FilePath -> IO (Either ParseErr a)
parse = evalKangaroo

runKangaroo :: Kangaroo st a -> st -> FilePath -> IO (Either ParseErr a,st)
runKangaroo p st filename = runGenKangaroo p st filename

-- answer, no state
evalKangaroo :: Kangaroo st a -> st -> FilePath -> IO (Either ParseErr a)
evalKangaroo = liftM fst `ooo` runKangaroo

-- state, no answer
execKangaroo :: Kangaroo st a -> st -> FilePath -> IO st
execKangaroo = liftM snd `ooo` runKangaroo


put :: st -> Kangaroo st ()
put = putUserSt

get :: Kangaroo st st
get = getUserSt

modify :: (st -> st) -> Kangaroo st ()
modify = modifyUserSt

gets :: (st -> a) -> Kangaroo st a
gets f = liftM f get