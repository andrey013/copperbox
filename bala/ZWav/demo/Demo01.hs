{-# OPTIONS -Wall #-}


module Demo01 where

import ZWav.ReadFile
import ZWav.WriteFile

import Control.Exception
import Prelude hiding (catch)

import System.Exit

test01 :: IO ()
test01 = process "wavfiles/min.wav"

test02 :: IO ()
test02 = process "wavfiles/sine_44k_s.wav"

process :: FilePath -> IO ()
process filename = do
    ans <- catch (readWav filename) exitHandle
    print ans
  where
    exitHandle :: IOException -> IO a 
    exitHandle e = putStrLn (show e) >> exitFailure

 