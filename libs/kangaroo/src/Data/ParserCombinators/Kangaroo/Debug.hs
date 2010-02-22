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
    debugHexAll
  , debugHexRange

  ) where


import Text.PrettyPrint.JoinPrint

import Data.Array.IO
import Data.Word

debugHexAll :: IOUArray Int Word8 -> IO ()
debugHexAll arr = getBounds arr    >>= \(s,e) -> 
                 hexdumpA s e arr >>= (putStrLn . show)


debugHexRange :: (Int,Int) -> IOUArray Int Word8 -> IO ()
debugHexRange (s,e) arr = hexdumpA s e arr >>= (putStrLn . show)
