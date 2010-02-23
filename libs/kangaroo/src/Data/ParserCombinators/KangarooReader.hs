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

  -- * Parser tpye 
    Kangaroo
    
  , parse
  , runKangaroo
  , ask
  , asks
  , local

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

import Control.Monad ( liftM )

type Kangaroo r a = GenKangaroo r a


parse :: Kangaroo r a
      -> r
      -> FilePath 
      -> IO (Either ParseErr a)
parse = runKangaroo 

runKangaroo :: Kangaroo r a
            -> r
            -> FilePath 
            -> IO (Either ParseErr a)
runKangaroo p env filename = liftM fst (runGenKangaroo p env filename)

-- | Retrieve the environment.
--
ask :: Kangaroo r r
ask = getUserSt

asks :: (r -> a) -> Kangaroo r a
asks f = liftM f getUserSt

-- | Execute a computation in a modified environment.
--
local :: (r -> r) -> Kangaroo r a -> Kangaroo r a
local f ma = do 
    e  <- getUserSt 
    putUserSt (f e)
    ans <- ma
    putUserSt e
    return ans

