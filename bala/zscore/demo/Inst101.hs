{-# OPTIONS -Wall #-}

-- 101 instrument from the Csound book...

module Inst101 where


import ZScore.CsoundInst
import ZScore.Opcodes
import ZScore.Utils.FormatCombinators

demo01 :: Doc
demo01 = format $ runInstBuilder 199 $ do 
   iamp <- ivar $ pfield 4
   ifrq <- ivar $ cpspch (pfield 5)    
   a1   <- oscil iamp ifrq 1
   out a1




