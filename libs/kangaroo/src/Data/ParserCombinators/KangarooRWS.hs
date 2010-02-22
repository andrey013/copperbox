{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Data.ParserCombinators.KangarooRWS
-- Copyright   :  (c) Stephen Tetley 2009-2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Kangaroo parse monad with user env, logging and state.
--
--------------------------------------------------------------------------------

module Data.ParserCombinators.KangarooRWS
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
  , tell
  , ask

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
  , printHexRange
  , printRegionStack 

  , module Data.ParserCombinators.Kangaroo.Combinators
  , module Data.ParserCombinators.Kangaroo.Prim

  ) where

import Data.ParserCombinators.Kangaroo.Combinators
import Data.ParserCombinators.Kangaroo.ParseMonad
import Data.ParserCombinators.Kangaroo.Prim
import Data.ParserCombinators.Kangaroo.Utils

import Control.Monad
import Data.Monoid

type Kangaroo r w st a = GenKangaroo (r,w,st) a


values :: (a,(r,w,st)) -> (a,w,st)
values (ans,(_,w,ust)) = (ans,w,ust)


state3 :: (r,w,st) -> st
state3 (_,_,st) = st

env3 :: (r,w,st) -> r
env3 (r,_,_) = r

parse :: Monoid w 
      => Kangaroo r w st a 
      -> r 
      -> st 
      -> FilePath 
      -> IO (Either ParseErr a)
parse = liftM fst `oooo` evalKangaroo 

runKangaroo :: Monoid w 
            => Kangaroo r w st a 
            -> r 
            -> st 
            -> FilePath 
            -> IO (Either ParseErr a,w,st)
runKangaroo p env st filename = 
    liftM values $ runGenKangaroo p (env,mempty,st) filename

-- answer, no state
evalKangaroo :: Monoid w 
             => Kangaroo r w st a 
             -> r 
             -> st 
             -> FilePath 
             -> IO (Either ParseErr a,w)
evalKangaroo = liftM fn `oooo` runKangaroo where
    fn (a,w,_) = (a,w)

-- state, no answer
execKangaroo :: Monoid w 
             => Kangaroo r w st a 
             -> r 
             -> st 
             -> FilePath 
             -> IO st
execKangaroo = liftM state3 `oooo` runKangaroo 
 

put :: st -> Kangaroo r w st ()
put st = getUserSt >>= \(r,w,_) -> putUserSt (r,w,st)

get :: Kangaroo r w st st
get = liftM state3 getUserSt 

modify :: (st -> st) -> Kangaroo r w st ()
modify f = getUserSt >>= \(r,w,st) -> putUserSt (r,w,f st)

gets :: (st -> a) -> Kangaroo r w st a
gets f = liftM f get

tell :: Monoid w => w -> Kangaroo r w st ()
tell s = getUserSt >>= \(r,w,st) -> putUserSt (r,w `mappend` s,st)

ask :: Kangaroo r w st r
ask = liftM env3 getUserSt