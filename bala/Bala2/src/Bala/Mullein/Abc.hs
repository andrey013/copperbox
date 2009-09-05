{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Mullein.Abc
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- ABC stuff
--
--------------------------------------------------------------------------------

module Bala.Mullein.Abc
  ( 

  -- * Write temp file and render to ABC
    abcBuffer  
  ) where

import Mullein.Abc

import System.Process ( system )
import Text.PrettyPrint.Leijen


--------------------------------------------------------------------------------
-- Duration



abcBuffer :: Doc -> IO ()
abcBuffer doc = do 
  writeDoc "temporary.abc" doc
  system "abcm2ps -O temporary.ps temporary.abc"
  return ()