
module Ext.Caml.AstConstruction where

import Gen.OCamlAbsSyn
import qualified Base.Lib as B

constrCode :: Int -> [(String, B.Cardinality)] -> Expr
constrCode i xs = enseq (ptag i : xs')
  where xs' = fst $ foldr foldStep ([],length xs) xs


tupleCode :: [(String, B.Cardinality)] -> Expr
tupleCode []                = error "noArgs"
tupleCode [(tyname,card)]   = singleStep tyname card "x"
tupleCode xs                = Match (var "x") [(pat1, Nothing, expr1)]
  where
    pat1  = patTuple (length xs) Nothing
    expr1 = enseq (fst $ foldr foldStep ([],length xs) xs)


foldStep ::  (String,B.Cardinality) -> ([Expr],Int) -> ([Expr],Int)
foldStep (pklrname, card) (xs,i) = (a:xs,i-1)
  where a = singleStep pklrname card ('x' : show i)
    
                                        
singleStep :: String -> B.Cardinality -> String -> Expr
singleStep pklrname card varname = (App (picklerApplication pklrname card) args)
  where
    args = [ArgExpr (var varname), ArgExpr (var "s")]
    

picklerApplication :: String -> B.Cardinality -> Expr
picklerApplication pklrname card = fn card
  where
    fn B.One = (var pklrname)
    fn B.Opt = inner (var "pmaybe")
    fn B.Zom = inner (var "plist")
    
    inner v = ParenExpr (App v [ArgExpr (var pklrname)])

recordCode :: [(String, B.Cardinality, (Maybe String))] -> Expr
recordCode xs = enseq (map pklField xs)


pklField (pklr_name, card, Nothing)       = error $ "pklField - " ++ pklr_name
pklField (pklr_name, card, Just fld_name) = (App (picklerApplication pklr_name card) args)
  where 
    args = [ArgExpr field_access, ArgExpr (var "s")]
    field_access = fieldAccess (var "x") (field fld_name)
     

fieldAccess v fld = FieldAccess v fld    
    


var str = ValuePathExpr (Nothing, str)

field str = (Nothing, str)

constr str = (Nothing, str)  


varTuple i = TupleExpr vars
  where vars = [ var ('x':show x) | x <- [1..i] ]

patTuple i tyexp = ParenPat (TuplePat pats) tyexp
  where pats = [ ValuePat ('x':show x) | x <- [1..i] ]

patConstr :: String -> Int -> Pattern
patConstr name 0  = ValuePat name
patConstr name i  = ConstrPat (constr name) (patTuple i Nothing)



matchi :: Int -> Expr
matchi i = Match (varTuple i) []



arg :: String -> Argument 
arg s = ArgExpr (var s)

param :: String -> Parameter 
param s = ParamPat (ValuePat s)

enseq :: [Expr] -> Expr
enseq []      = App (ValuePathExpr (Nothing , "raise")) []
enseq [a]     = a
enseq (x:xs)  = enseq' xs x 
  where enseq' [a]    k = (k `Seq` a)
        enseq' (a:xs) k = enseq' xs (k `Seq` a)
        


ptag :: Int -> Expr
ptag i = App (ValuePathExpr (Nothing , "ptag")) 
             [ (ArgExpr (ConstantExpr (IntegerLiteral i)))
             , (ArgExpr (ValuePathExpr (Nothing , "s")))
             ] 
             
             
