{-# LANGUAGE ScopedTypeVariables        #-}
{-# OPTIONS -Wall #-}

-- 101 instrument from the Csound book...

module Xanadu where

import XanaduInst


import ZSnd.Basic.Kernel
import ZSnd.Basic.Symbolic.Pitch

import ZSnd.Core                                -- package: zsnd-core


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
sco01 = execScore $ execNotelist ctx_zero notelist1

notelist1 :: Notelist X3Ctx Double ()
notelist1 = do 
   eventl 0 $ fgen10  8192 [1]
   eventl 0 $ fgen11  8192 1
   eventl 0 $ fgenN12 8192 20

   eventl 0 $ note 7.06 15
   eventl 0 $ note 8.01 15
   eventl 0 $ note 8.06 15
   eventl 0 $ note 8.10 15
   eventl 0 $ note 8.11 15
   eventl 0 $ note 9.04 15





-- Gen21 probably makes sense as a family of LocEvents.

--    dyninst 1 3 3 []

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


instance CtxTempo X3Ctx where
  tempo         = x3_tempo
  set_tempo i s = s { x3_tempo = i }


note :: InterpretUnit u => Pitch -> Double -> ULocEvent X3Ctx u
note pch drn = promoteLoc $ \u -> 
    askCtx >>= \ctx -> 
    let du = normalize (tempo ctx) u
    in primEvent (prim1 $ NoteStmt { onset_time = du
                                   , event_dur  = drn
                                   , event_gen  = mk ctx })
  where
    mk ctx = \ot dx -> dyninst 3 ot dx [0, pch, (line_start ctx), (line_end ctx)]
  