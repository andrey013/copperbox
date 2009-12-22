{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Picklers
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Pickling
--
--------------------------------------------------------------------------------

module Picklers where


import qualified Data.ByteString.Char8 as BS
import Data.Char
import Data.List ( foldl' )
import Data.Word
import System.IO

newtype RevBS = RevBS { getRevBS :: BS.ByteString }

type Pickle = RevBS

empty :: RevBS 
empty = RevBS BS.empty


writePickle :: FilePath -> Pickle -> IO ()
writePickle file pkl = withBinaryFile file WriteMode (BS.hPut `flip` rpkl)
  where rpkl = BS.reverse $ getRevBS pkl 


cons :: Char -> Pickle -> Pickle
cons c = RevBS . BS.cons c . getRevBS 

putCString :: String -> Pickle -> Pickle
putCString s pkl = putWord8 0 $ foldl' (flip cons) pkl s

putWord8 :: Word8 -> Pickle -> Pickle
putWord8 w = cons (chr $ fromIntegral w)

-- TODO 
cstring :: String -> Pickle
cstring = putCString `flip` empty 