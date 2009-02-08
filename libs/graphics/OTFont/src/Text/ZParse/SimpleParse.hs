{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Text.ZParse.SimpleParse
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- SimpleParser - run the ParseMonad transformer over the identity monad 
--
--------------------------------------------------------------------------------


module Text.ZParse.SimpleParse where

import Text.ZParse.ParseMonad

import Control.Monad.Identity


type Parser st a = ParserT st Identity a

runParser :: (Show pos, ParseState st pos inp) => Parser st a -> st -> a
runParser m st0 = runIdentity $ 
    unParserT m (\sk _ _ -> return sk) 
                (\st    -> error $ "error - runSimple at " ++ show (pos st) )
                st0
                                   