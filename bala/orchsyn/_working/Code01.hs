{-# OPTIONS -Wall #-}


module Code01 where

import Text.PrettyPrint.HughesPJ


-- Unit = ()

-- No Sym for this code.


-- | Simplest expression language, not even capable of describing
-- instr1 in the Csound book.
--
-- Constants (Pfield, Signal) and application (Oscil, Out).
--
-- Note - no @let@ form which means we cannot generate valid
-- instruments just by pretty printing. Csound is not an 
-- expression language - opcode calls cannot call other opcodes 
-- in their arguments.
-- 
data Expr = Pfield Int
          | Signal Double
          | Oscil Expr Expr Expr
          | Out Expr
  deriving (Eq,Show)
          
out :: Expr -> Expr
out = Out

oscil :: Expr -> Expr -> Int -> Expr
oscil amp hz ifn = Oscil amp hz (Pfield ifn)


signal :: Double -> Expr
signal = Signal

code01 :: Expr
code01 = out a1
  where
    a1 = oscil (signal 1.0) (signal 440.0) 1

demo01 :: Doc
demo01 = ppExpr code01

ppExpr :: Expr -> Doc
ppExpr (Pfield i)       = int i
ppExpr (Signal d)       = double d
ppExpr (Oscil e1 e2 e3) = 
    text "oscil" <+> ppExpr e1 <+> ppExpr e2 <+> ppExpr e3
ppExpr (Out e)          = text "out" <+> ppExpr e