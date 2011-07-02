{-# OPTIONS -Wall #-}

-- 101 instrument from the Csound book...

module Demo101 where


import ZSnd.Core
import ZSnd.Core.Opcodes

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
orch01 = Orch default_mono_header [inst1]
  where
    inst1 = runInstU 101 0 $ do 
      a1   <-  alet $ oscil 1 $ port0_2 (10000, 440)
      o1   <-  alet $ out1 $ port1_1 id
      a1   =>= o1
      out o1
  
sco01 :: Section
sco01 = runScoBuilder $ do 
    gen10   0 4096 [1]  
    dyninst 101 0 3 []
--    dyninst 101 3 3 []
