{-# LANGUAGE MultiParamTypeClasses      #-}
{-# OPTIONS -Wall #-}

-- 101 instrument from the Csound book...

module Demo101 where


import Majalan.Core

import System.Directory
import System.Process


main :: IO ()
main = do
    createDirectoryIfMissing True "./out/" 
    writeUnifiedFile "out/cs101.csd" flags orch01 (runScore env1 my_sco)
    _ <- system "csound out/cs101.csd"
    return ()
  where
    flags = flags_wav_file_out "out/cs101.wav"

    my_sco = sco01 `scoOver` sco01



sco01 :: Instr101 env => RScore env
sco01 = prefixGens [ gen10 1 4096 [1] ] $ 
          frame [ i101    0 0.75 
                , i101    1 0.50
                , i101    2 0.25
                ]


--
-- Scores support /link time/ resolution of instrument 
-- numbers (and f-tables)...
--


data MyEnv = MyEnv  { instr101_num :: Int }

env1 :: MyEnv
env1 = MyEnv { instr101_num = 101 }

instance Instr101 MyEnv where
  asksInstr101 e = instr101_num e


-- | st * dur
--
i101 :: Instr101 env => Double -> Double -> Note env
i101 start dur = absNote asksInstr101 start dur [] 

class Instr101 env where
  asksInstr101 :: env -> Int

--
-- Note - usually orchestras will be incorporated from 
-- pre-existing files...
--

orch01 :: [String]
orch01 = [instr101]

instr101 :: String
instr101 = unlines $ 
    [ "instr 101"
    , "a1      oscil       10000, 440, 1"
    , "        out         a1"
    , "endin"
    ]


