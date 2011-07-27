{-# OPTIONS -Wall #-}

-- Xanadu instrument instrument from the Csound distribution.

module Xanadu where


import Majalan.Basic.Kernel

import Majalan.Core                             -- package: zsnd-core


import System.Process
import System.Directory


main :: IO ()
main = do
    createDirectoryIfMissing True "./out/" 
    writeUnifiedScore "out/xanadu.csd" flags "xanadu3.orc" sco01
    _ <- system "csound out/xanadu.csd"
    return ()
  where
    flags = flags_wav_file_out "out/xanadu.wav"



-- sco01 :: PrimScore
sco01 = runScore (ITable 3) $ withGens gs $ traceNotelistU ctx notelist1
  where
    ctx = initialContext ctx_zero
    gs  = [ gen10  1 8192 [1]
          , gen11  2 8192 1
          , genN12 3 8192 20 ]

notelist1 :: Notelist ITable X3Ctx Double ()
notelist1 = do 
    eventl 0 $ localize (staccato_factor 0.5) $ execTraceLoc $ do
      note 7.06 15
      note 8.01 15
      note 8.06 15
      note 8.10 15
      note 8.11 15
      note 9.04 15


note :: Double -> Double -> TraceLoc ITable X3Ctx Double ()
note pch dur = 
    insertl (noteEvt pch dur) >> moveBy dur



-- xan3
-- 
-- > p1 (inst) , p2 (onset), p3 (duration)
--
-- > p4 appears unused (p4 is commonly amplitude) 
-- > p5 freq (oct-point-pitch-class)
-- > p6 line seg start
-- > p7 line seg end
--

type Pitch = Double 

data ITable = ITable { instr3_num      :: Int }
       

data X3Ctx = X3Ctx
      { x3_tempo        :: Tempo
      , line_start      :: Double
      , line_end        :: Double
      }

ctx_zero :: X3Ctx
ctx_zero = X3Ctx { x3_tempo        = 120
                 , line_start      = 2.0
                 , line_end        = 0.2
                 }


noteEvt :: Double -> Double -> ULocEvent ITable X3Ctx Double
noteEvt pch drn =  
    get_user_context >>= \ctx -> 
    primULocEvent instr3_num drn [ CsInt 0
                                 , CsDouble pch
                                 , CsDouble $ line_start ctx
                                 , CsDouble $ line_end ctx ]
  