
-- need a naming convention apropos THiH  

module LambdaCore.Construct where

import Gen.LambdaCore.LambdaCoreAbsSyn 


demo_core :: Program
demo_core
  = Program [writeFun "Global" "Global" "GType" 1 ["TypeInfo","Location"]]
  



  
writeFun name t_name c_name tag e_names 
  = Let [TySig writeName (tWriter t_name)] body
  where writeName = "write" ++ name
        body = Lam (ConP c_name ps, TyCon t_name) instrs
        ps = let xs = ["x" ++ show i | i <- [1..(length e_names)]]
             in map VarP xs
        instrs = eSeqn $ write_tag : map eWriter (zip e_names [1..])
        write_tag = App (Var "write_tag") (Const (IntL tag))




 
tWriter :: String -> TyExpr
tWriter name = TyApp (TyCon name) tUnit

eWriter :: (String, Int) -> Expr
eWriter (name,i) = App (Var $ "write" ++ name) (Var $ "x" ++ show i)

eSeq :: Expr -> Expr -> Expr
eSeq e e' = (App (Lam (VarP "x", TyCon "Unit") e') e)

eSeqn :: [Expr] -> Expr
eSeqn [e]     = e
eSeqn (e:es)  = foldl eSeq e es

tUnit :: TyExpr
tUnit = TyCon "Unit"

app fname expr = App (Var fname) expr
