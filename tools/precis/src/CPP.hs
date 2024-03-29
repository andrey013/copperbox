{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  CPP
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  LGPL (depends on CppHs)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Preprocess with HSCPP
--
--------------------------------------------------------------------------------


module CPP 
  (
    preprocessFile
  , precisCpphsOptions
  ) where

import Precis.HsSrc.Datatypes

import Language.Preprocessor.Cpphs


preprocessFile :: CpphsOptions -> FilePath -> IO MacroExpandedSrcFile
preprocessFile opts file_name = do
  input  <- readFile file_name
  output <- runCpphs opts file_name input
  return $ MacroExpandedSrcFile file_name output


precisCpphsOptions :: CpphsOptions
precisCpphsOptions = updBoolOpts (\s -> s {hashline = False}) defaultCpphsOptions

    
updBoolOpts :: (BoolOptions -> BoolOptions) -> CpphsOptions -> CpphsOptions
updBoolOpts f b = let opts = boolopts b in b { boolopts = f opts }



