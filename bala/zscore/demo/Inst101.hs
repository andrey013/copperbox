{-# OPTIONS -Wall #-}

-- 101 instrument from the Csound book...

module Inst101 where


import ZScore.CsoundInst
import ZScore.FormatCombinators

demo01 :: Doc
demo01 = runInstBuilder 199 $ do 
   iamp <- var "iamp" $ pfield 4
   ifrq <- var "ifrq" $ cpspch (pfield 5)    
   a1   <- oscil "a1" [iamp, ifrq, 1] 
   out a1
   return ()


oscil :: String -> [Expr] -> InstBuilder Expr
oscil name = opcode name "oscil"



