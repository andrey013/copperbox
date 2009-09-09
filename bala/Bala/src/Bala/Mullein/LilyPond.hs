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
  -- * Write a file and render to LilyPond 
    runLilyPond 
  ) where

import Mullein.LilyPond

import System.Process ( system )
import Text.PrettyPrint.Leijen


--------------------------------------------------------------------------------
-- Duration




runLilyPond :: FilePath -> Doc -> IO ()
runLilyPond path doc = do 
  writeDoc path doc
  system $ "lilypond " ++ path
  return ()