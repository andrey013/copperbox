{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Precis.CPP
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Preprocess with HSCPP
--
--------------------------------------------------------------------------------


module Precis.CPP where

import Language.Preprocessor.Cpphs


preproUseless :: FilePath -> IO String
preproUseless = preprocessFile defaultCpphsOptions


preprocessFile :: CpphsOptions -> FilePath -> IO String
preprocessFile opts file_name = do
  input  <- readFile file_name
  output <- runCpphs opts file_name input
  return output

  

{-
-- parseModule 

parseModuleWithExts :: [Extension] -> FilePath -> String -> ParseResult Module
parseModuleWithExts exts file_name txt = 
    parseModuleWithMode mode txt
  where
    mode = defaultParseMode { extensions = exts, parseFilename = filename }
-}