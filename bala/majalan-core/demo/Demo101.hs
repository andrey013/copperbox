{-# LANGUAGE MultiParamTypeClasses      #-}
{-# OPTIONS -Wall #-}

-- 101 instrument from the Csound book...

module Demo101 where


import Majalan.Core
import qualified Majalan.Core.NoteList as N
import Majalan.Core.NoteList ( CsValue(..) )

import System.Directory
import System.Process

import qualified Data.ByteString.Char8 as B


dummy = N.printEvents [e1,e2,e3] (N.columnSpecs [])
  where
    e1 = N.CsEvent 101 0 [D 0.75, I 1, D 10.0]
    e2 = N.CsEvent 101 1 [D 0.50, I 1, D 12.0]
    e3 = N.CsEvent 101 2 [D 0.25, I 1, D 14.5]


main :: IO ()
main = do
    createDirectoryIfMissing True "./out/" 
    writeUnifiedFile "out/cs101.csd" flags orch01 cols sco01
    _ <- system "csound out/cs101.csd"
    return ()
  where
    flags = flags_wav_file_out "out/cs101.wav"


cols = columnSpecs []

sco01 :: Score
sco01 = Score [ gen10 1 4096 [1] ] $ 
              [ i101    0 0.75 
              , i101    2 0.50
              , i101    4 0.25
              ]


-- | st * dur
--
i101 :: Double -> Double -> CsEvent
i101 start dur = CsEvent 101 start [D dur]



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


