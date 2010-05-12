{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Precis.TextOutput
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Print ChangeStats to the console.
--
--------------------------------------------------------------------------------


module Precis.TextOutput
  ( 
    writeChangeStats
  , comparingMsg
  ) where

import Precis.Datatypes
import Precis.PPShowS
import Precis.ReportMonad


writeChangeStats :: ChangeStats -> IO ()
writeChangeStats _ = return ()


comparingMsg :: CabalPrecis -> CabalPrecis -> String
comparingMsg new old = toString $ suffixEllipses $ hsep $ map text 
    [ "Comparing", package_name new, package_version new
    , "to",        package_name old, package_version old
    ]
  where
    suffixEllipses = (<> text "...")
