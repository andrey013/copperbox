{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Mullein.LilyPond
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- LilyPond stuff
--
--------------------------------------------------------------------------------

module Bala.Mullein.LilyPond
  ( 
  -- * Write temp file and render to LilyPond 
    lilyPondRun
  ) where

import Mullein.LilyPond

import System.Process ( system )
import Text.PrettyPrint.Leijen


--------------------------------------------------------------------------------
-- Duration




lilyPondRun :: Doc -> IO ()
lilyPondRun doc = do 
  writeDoc "temporary.ly" doc
  system "lilypond temporary.ly"
  return ()