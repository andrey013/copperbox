{-# LANGUAGE ScopedTypeVariables        #-}
{-# OPTIONS -Wall #-}

-- Xanadu instrument instrument from the Csound distribution.

module Xanadu where

import XanaduInst


import ZSnd.Basic.Kernel

import ZSnd.Core                                -- package: zsnd-core

import Data.AffineSpace

import System.Process
import System.Directory

main :: IO ()
main = do
    createDirectoryIfMissing True "./out/" 
    writeUnifiedFile "out/xanadu.csd" flags xanadu_orc sco01
    _ <- system "csound out/xanadu.csd"
    return ()
  where
    flags = flags_wav_file_out "out/xanadu.wav"



sco01 :: Score
sco01 = traceNotelistU (initialContext ctx_zero) notelist1

notelist1 :: Notelist X3Ctx Double ()
notelist1 = do 
    eventl 0 $ fgen10  1 8192 [1]
    eventl 0 $ fgen11  2 8192 1
    eventl 0 $ fgenN12 3 8192 20

    eventl 0 $ note 7.06 15
    eventl 0 $ note 8.01 15
    eventl 0 $ note 8.06 15
    eventl 0 $ note 8.10 15
    eventl 0 $ note 8.11 15
    eventl 0 $ note 9.04 15




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



note :: InterpretUnit u => Double -> Double -> ULocEvent X3Ctx u
note pch drn = promoteLoc $ \ot -> 
    normalizeCtx ot  >>= \dot ->
    normalizeCtx drn >>= \ddrn -> 
    get_user_context >>= \uctx -> 
    primEvent $ prim1 $ absEvent dot (InstStmtProps 3 ddrn (mkPfs uctx))
  where
    mkPfs ctx = [ CsInt 0, CsDouble pch
                , CsDouble (line_start ctx), CsDouble (line_end ctx)]
  