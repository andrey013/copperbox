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


import qualified Data.ByteString as BS
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


cons :: Word8 -> Pickle -> Pickle
cons c = RevBS . BS.cons c . getRevBS 

putCString :: String -> Pickle -> Pickle
putCString s pkl = putWord8 0 $ foldl' (flip putChar8) pkl s

putWord8 :: Word8 -> Pickle -> Pickle
putWord8 = cons 

putChar8 :: Char -> Pickle -> Pickle
putChar8 ch = cons (fromIntegral $ ord ch)

-- TODO 
cstring :: String -> Pickle
cstring = putCString `flip` empty 


string :: String -> Pickle
string = foldl' (flip putChar8) empty