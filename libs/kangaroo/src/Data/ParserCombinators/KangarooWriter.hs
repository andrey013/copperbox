{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Data.ParserCombinators.KangarooWriter
-- Copyright   :  (c) Stephen Tetley 2009, 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Kangaroo parse monad with logging.
--
--------------------------------------------------------------------------------

module Data.ParserCombinators.KangarooWriter
  (

    Kangaroo
  , parse
  , runKangaroo
  , tell

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

import Data.Monoid

type Kangaroo r a = GenKangaroo r a


parse :: Monoid w 
      => Kangaroo w a  
      -> FilePath 
      -> IO (Either ParseErr a,w)
parse = runKangaroo 

runKangaroo :: Monoid w 
            => Kangaroo w a
            -> FilePath 
            -> IO (Either ParseErr a,w)
runKangaroo p filename = runGenKangaroo p mempty filename

tell :: Monoid w => w -> Kangaroo w ()
tell s = getUserSt >>= \w -> putUserSt $ w `mappend` s
