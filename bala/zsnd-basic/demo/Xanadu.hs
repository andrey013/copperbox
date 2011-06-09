{-# LANGUAGE ScopedTypeVariables        #-}
{-# OPTIONS -Wall #-}

-- 101 instrument from the Csound book...

module Xanadu where

import XanaduInst


import ZSnd.Basic.Kernel
import ZSnd.Core

import System.Process
import System.Directory

main :: IO ()
main = do
    createDirectoryIfMissing True "./out/" 
    writeUnifiedFile "out/xanadu.csd" flags xanadu_orc [sco01]
    _ <- system "csound out/xanadu.csd"
    return ()
  where
    flags = flags_wav_file_out "out/xanadu.wav"



sco01 :: Section
sco01 = runScoBuilder $ do 
    gen10   0 8192 [1]  
    gen11   0 8192 1
    gen12_  0 8192 20

    dyninst 3 0 15 [0, 7.06, 2.0, 0.2]
    dyninst 3 0 15 [0, 8.01, 2.0, 0.2]
    dyninst 3 0 15 [0, 8.06, 2.0, 0.2]
    dyninst 3 0 15 [0, 8.10, 2.0, 0.2]
    dyninst 3 0 15 [0, 8.11, 2.0, 0.2]
    dyninst 3 0 15 [0, 9.04, 2.0, 0.2]

--    dyninst 1 3 3 []

