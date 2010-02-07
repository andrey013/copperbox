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
    module Data.ParserCombinators.Kangaroo.Combinators
  , module Data.ParserCombinators.Kangaroo.ParseMonad
  , module Data.ParserCombinators.Kangaroo.Prim
  , Kangaroo
  , parse
  , runKangaroo
  , tell

  ) where

import Data.ParserCombinators.Kangaroo.Combinators
import Data.ParserCombinators.Kangaroo.ParseMonad hiding 
    ( GenKangaroo, getUserSt, putUserSt, modifyUserSt )
import qualified Data.ParserCombinators.Kangaroo.ParseMonad as PM
import Data.ParserCombinators.Kangaroo.Prim

import Data.Monoid

type Kangaroo r a = PM.GenKangaroo r a


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
tell s = PM.getUserSt >>= \w -> PM.putUserSt $ w `mappend` s
