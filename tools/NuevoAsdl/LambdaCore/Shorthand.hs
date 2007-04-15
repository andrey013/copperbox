
module LambdaCore.Shorthand where

import Gen.LambdaCore.LambdaCoreAbsSyn 


tFn:: TyExpr -> TyExpr -> TyExpr
tFn t1 t2 = TyApp t1 t2

tUnit :: TyExpr
tUnit = TyCon "Unit"

eSeq :: Expr -> Expr -> Expr
eSeq e e' = (App (Lam (VarP "x", TyCon "Unit") e') e)

eSeqn :: [Expr] -> Expr
eSeqn [e]     = e
eSeqn (e:es)  = foldl eSeq e es