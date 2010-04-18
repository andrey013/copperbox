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

  -- * Write a file and render to ABC
    runABC
  ) where

import Mullein.Abc

import System.FilePath
import System.Process ( system )
import Text.PrettyPrint.Leijen


--------------------------------------------------------------------------------
-- Duration



runABC :: FilePath -> Doc -> IO ()
runABC path doc = let pathPS = makePSFilePath path in do 
  writeDoc path doc
  system $ "abcm2ps -O " ++ pathPS ++ " " ++ path 
  return ()


makePSFilePath :: FilePath -> FilePath
makePSFilePath = (addExtension `flip` "ps") . dropExtension