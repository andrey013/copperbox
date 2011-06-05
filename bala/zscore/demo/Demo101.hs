{-# OPTIONS -Wall #-}

-- 101 instrument from the Csound book...

module Demo101 where


import ZScore
import ZScore.Opcodes

import System.Directory
import System.Process

main :: IO ()
main = do
    createDirectoryIfMissing True "./out/" 
    writeUnifiedFile "out/cs101.csd" flags orch01 [sco01]
    _ <- system "csound out/cs101.csd"
    return ()
  where
    flags = flags_wav_file_out "out/cs101.wav"

orch01 :: Orch
orch01 = Orch default_orch_header [inst1]
  where
    inst1 = runInstBuilder 1 $ do 
      a1   <- oscil 10000 440 1
      out a1

sco01 :: Section
sco01 = runScoBuilder $ do 
    gen10   1 0 4096 [1]  
    dyninst 1 0 3 []
--    dyninst 1 3 3 []
