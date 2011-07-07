{-# OPTIONS -Wall #-}

-- 101 instrument from the Csound book...

module Demo101 where


import ZSnd.Core
import ZSnd.Core.Opcodes

import System.Directory
import System.Process


dummy01 = printScore (sco01 `scoOver` sco01)

main :: IO ()
main = do
    createDirectoryIfMissing True "./out/" 
    writeUnifiedFile "out/cs101.csd" flags orch01 (sco01 `scoOver` sco01)
    _ <- system "csound out/cs101.csd"
    return ()
  where
    flags = flags_wav_file_out "out/cs101.wav"

orch01 :: Orch
orch01 = runOrchU default_mono_header body
  where
    body = instr 101 $ do 
      a1   <-  alet $ oscil (10000::IR) (440::IR) (tablefn 1)
      out (var a1)

sco01 :: Score 
sco01 = frame [ absTableGen 0 $ gen10 1 4096 [1] 
              , absEvent    0 $ InstStmtProps 101 0.75 [] 
              , absEvent    1 $ InstStmtProps 101 0.50 [] 
              , absEvent    2 $ InstStmtProps 101 0.25 [] 
              ]

{-
  
sco01 :: Section
sco01 = runScoBuilder $ do 
    gen10   0 4096 [1]  
    dyninst 101 0 3 []
--    dyninst 101 3 3 []

-}