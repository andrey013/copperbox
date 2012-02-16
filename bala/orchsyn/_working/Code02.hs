{-# OPTIONS -Wall #-}


module Code02 where

import Text.PrettyPrint.HughesPJ


-- Unit = ()


-- | Adding @let@ and @var@ - we now have an expression language
-- capable of describing instr1 in the Csound book.
-- 
data Expr = Pfield Int
          | Signal Double
          | Var Var
          | Oscil Expr Expr Expr
          | Out Expr
          | Let Var Expr Expr
  deriving (Eq,Show)

type Var = String
          
out :: Expr -> Expr
out = Out

oscil :: Expr -> Expr -> Int -> Expr
oscil amp hz ifn = Oscil amp hz (Pfield ifn)


signal :: Double -> Expr
signal = Signal

letE :: String -> Expr -> Expr -> Expr
letE = Let

rval :: String -> Expr 
rval = Var

code01 :: Expr
code01 = letE "a1" a1 (out (rval "a1"))
  where
    a1 = oscil (signal 1.0) (signal 440.0) 1

demo01 :: Doc
demo01 = ppExpr code01

ppExpr :: Expr -> Doc
ppExpr (Pfield i)       = int i
ppExpr (Signal d)       = double d
ppExpr (Var s)          = text s
ppExpr (Oscil e1 e2 e3) = 
    text "oscil" <+> ppExpr e1 <+> ppExpr e2 <+> ppExpr e3
ppExpr (Out e)          = text "out" <+> ppExpr e
ppExpr (Let v e b)      = ppLet v (ppExpr e) (ppExpr b)

ppLet :: String -> Doc -> Doc -> Doc
ppLet v e b = text "let" <+> text v <+> char '=' <+> e <+> text "in" <+> b