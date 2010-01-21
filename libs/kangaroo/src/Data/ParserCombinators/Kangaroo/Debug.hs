{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Data.ParserCombinators.Kangaroo.Debug
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Debug helpers
--
--------------------------------------------------------------------------------

module Data.ParserCombinators.Kangaroo.Debug
  (
    slowHexAll
  
  ) where


import Text.PrettyPrint.JoinPrint

import Data.Array.IO
import Data.Word

slowHexAll :: IOUArray Int Word8 -> IO ()
slowHexAll arr = getElems arr  >>= \xs    -> 
                 getBounds arr >>= \(s,e) -> putStrLn $ render $ hexdump s e xs

