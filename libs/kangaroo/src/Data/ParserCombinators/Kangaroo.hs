{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Data.ParserCombinators.Kangaroo
-- Copyright   :  (c) Stephen Tetley 2009-2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Binary parser combinators with random access
--
--------------------------------------------------------------------------------

module Data.ParserCombinators.Kangaroo 
  (
    Kangaroo
  , runKangaroo
  , parse

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
  , skip

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
  , printHexRange
  , printRegionStack 
  
  , module Data.ParserCombinators.Kangaroo.Combinators
  , module Data.ParserCombinators.Kangaroo.Prim


  ) where

import Data.ParserCombinators.Kangaroo.Combinators
import Data.ParserCombinators.Kangaroo.ParseMonad
import Data.ParserCombinators.Kangaroo.Prim


type Kangaroo a = GenKangaroo () a

runKangaroo :: Kangaroo a -> FilePath -> IO (Either ParseErr a)
runKangaroo p filename = runGenKangaroo p () filename >>= \(a,_) -> return a

-- just runKangaroo here

parse :: Kangaroo a -> FilePath -> IO (Either ParseErr a)
parse = runKangaroo 

