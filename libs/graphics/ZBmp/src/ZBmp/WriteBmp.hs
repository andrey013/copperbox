{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZBmp.WriteBmp
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Write a bitmap to file.
--
--------------------------------------------------------------------------------

module ZBmp.WriteBmp where

import ZBmp.Datatypes

import qualified Data.ByteString.Lazy as B

import System.IO


type Output = B.ByteString

type BMPout = Output -> Output


writeBmp :: FilePath -> BMPfile -> IO ()
writeBmp path bmp = let bmpstream = putBmpFile bmp $ B.empty in do
    h <- openBinaryFile path WriteMode
    B.hPut h bmpstream
    hClose h  

putBmpFile :: BMPfile -> BMPout
putBmpFile = undefined

    