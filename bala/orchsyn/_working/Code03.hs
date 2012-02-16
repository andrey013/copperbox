{-# OPTIONS -Wall #-}


module Code03 where

import Text.PrettyPrint.HughesPJ


import Prelude hiding ( (*), (+) )

-- Unit = ()


-- | Generalizing Oscil and Out to Opcode adding BinOp.
-- 
data Expr = Pfield Int
          | Signal Double
          | Var Var
          | Opcode String [Expr]
          | BinOp String Expr Expr
          | Let Var Expr Expr
  deriving (Eq,Show)

type Var = String
          
out :: Expr -> Expr
out e = Opcode "out" [e]

oscil :: Expr -> Expr -> Int -> Expr
oscil amp hz ifn = Opcode "oscil" [ amp, hz, Pfield ifn ]


signal :: Double -> Expr
signal = Signal

letE :: String -> Expr -> Expr -> Expr
letE = Let

rval :: String -> Expr 
rval = Var

infixl 7 *

(*) :: Expr -> Expr -> Expr
(*) = BinOp "*"

infixl 6 +

(+) :: Expr -> Expr -> Expr
(+) = BinOp "+"


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
ppExpr (Opcode s es)    = text s <+> ppArgs es
ppExpr (BinOp s e1 e2)  = ppExpr e1 <+> text s <+> ppExpr e2
ppExpr (Let v e b)      = ppLet v e b

ppLet :: String -> Expr -> Expr -> Doc
ppLet v e b = 
    text "let" <+> text v <+> char '=' <+> ppExpr e <+> text "in" <+> ppExpr b

ppArgs :: [Expr] -> Doc
ppArgs = hsep . map ppExpr