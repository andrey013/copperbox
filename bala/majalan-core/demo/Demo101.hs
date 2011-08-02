{-# LANGUAGE MultiParamTypeClasses      #-}
{-# OPTIONS -Wall #-}

-- 101 instrument from the Csound book...

module Demo101 where


import Majalan.Core

import System.Directory
import System.Process

import qualified Data.ByteString.Char8 as B

main :: IO ()
main = do
    createDirectoryIfMissing True "./out/" 
    either print sk $ runScoreM instr_tbl my_sco
    return ()
  where
    sk ans = do { writeUnifiedFile "out/cs101.csd" flags orch01 ans
                ; _ <- system "csound out/cs101.csd"
                ; return ()
                }
    flags = flags_wav_file_out "out/cs101.wav"

    my_sco = sco01 `scoOver` sco01



sco01 :: RScore
sco01 = prefixGens [ gen10 1 4096 [1] ] $ 
          frame [ i101    0 0.75 
                , i101    1 0.50
                , i101    2 0.25
                ]



instr_tbl :: InstrMap
instr_tbl = buildInstrMap [ (101, instr_name_101) ]

--
-- Scores support /link time/ resolution of instrument 
-- numbers (and f-tables)...
--

instr_name_101 :: B.ByteString
instr_name_101 = B.pack "instr_101_unique_name"

-- | st * dur
--
i101 :: Double -> Double -> Note
i101 start dur = absNote instr_name_101 start dur [] 



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


