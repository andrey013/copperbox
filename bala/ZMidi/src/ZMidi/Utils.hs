{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZMidi.Utils
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Utility functions
--
--------------------------------------------------------------------------------


module ZMidi.Utils (
    hex2
  , hex4
  , hex8
  ) where

import Numeric


--------------------------------------------------------------------------------

hex2 :: Integral a => a -> ShowS
hex2 a | a < 0      = showString "-ve"
       | a < 0x10   = showString "0x0" . showHex a
       | otherwise  = showString "0x"  . showHex a 


hex4 :: Integral a => a -> ShowS
hex4 a | a < 0      = showString "-ve"
       | a < 0x10   = showString "0x000" . showHex a
       | a < 0x100  = showString "0x00"  . showHex a 
       | a < 0x1000 = showString "0x0"   . showHex a 
       | otherwise  = showString "0x"    . showHex a 

hex8 :: Integral a => a -> ShowS
hex8 a | a < 0          = showString "-ve"
       | a < 0x10       = showString "0x0000000" . showHex a
       | a < 0x100      = showString "0x000000"  . showHex a 
       | a < 0x1000     = showString "0x00000"   . showHex a 
       | a < 0x10000    = showString "0x0000"    . showHex a 
       | a < 0x100000   = showString "0x000"     . showHex a 
       | a < 0x1000000  = showString "0x00"      . showHex a 
       | a < 0x10000000 = showString "0x0"       . showHex a 
       | otherwise      = showString "0x"        . showHex a 

