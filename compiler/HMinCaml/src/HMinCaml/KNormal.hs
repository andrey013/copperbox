

-- UUAGC 0.9.6 (KNormal.ag)


-- |
-- Module: HMinCaml.KNormal
-- License: as per original MinCaml
--
-- Maintainer: Stephen Tetley <stephen.tetley@gmail.com>
-- Stability: unstable
-- Portability: ghc
--
-- K Normal form datatypes
--


module HMinCaml.KNormal where


import HMinCaml.Id
import HMinCaml.KNormalSyn
import qualified HMinCaml.M as M
import qualified HMinCaml.S as S
import HMinCaml.Type




fv :: Expr -> S.S Id
fv Unit               = S.empty
fv (Int _)            = S.empty 
fv (Float _)          = S.empty 
fv (ExtArray _)       = S.empty
fv (Neg x)            = S.singleton x
fv (FNeg x)           = S.singleton x
fv (Add x y)          = S.ofList [x,y]
fv (Sub x y)          = S.ofList [x,y]
fv (FAdd x y)         = S.ofList [x,y]
fv (FSub x y)         = S.ofList [x,y]
fv (FMul x y)         = S.ofList [x,y]
fv (FDiv x y)         = S.ofList [x,y]
fv (Get x y)          = S.ofList [x,y]
fv (IfEq x y e1 e2)   = S.add x $ S.add y (S.union (fv e1) (fv e2)) 
fv (IfLE x y e1 e2)   = S.add x $ S.add y (S.union (fv e1) (fv e2))
fv (Let (x,_) e1 e2)  = S.union (fv e1) (S.remove x (fv e2))
fv (Var x)            = S.singleton x
fv (LetRec (Fundef (x,_) yts e1) e2) =
      let zs = S.diff (fv e1) (S.ofList (map fst yts)) in
      S.diff (S.union zs (fv e2)) (S.singleton x)
      
fv (App x ys)         = S.ofList (x:ys)
fv (Tuple xs)         = S.ofList xs 
fv (ExtFunApp _ xs)   = S.ofList xs
fv (Put x y z)        = S.ofList [x,y,z]
fv (LetTuple xs y e)  = S.add y (S.diff (fv e) (S.ofList (map fst xs)))
  
  
-- g env Syntax.Unit     = (Unit, TUnit)

freeVars :: Expr -> S.S Id
freeVars e = undefined -- return $ fst (g M.empty e)
 
-- Expr --------------------------------------------------------
{-
   alternatives:
      alternative Add:
         child x              : {Id}
         child y              : {Id}
      alternative App:
         child ref            : {Id}
         child args           : {[Id]}
      alternative ExtArray:
         child ref            : {Id}
      alternative ExtFunApp:
         child nref           : {Id}
         child args           : {[Id]}
      alternative FAdd:
         child x              : {Id}
         child y              : {Id}
      alternative FDiv:
         child x              : {Id}
         child y              : {Id}
      alternative FMul:
         child x              : {Id}
         child y              : {Id}
      alternative FNeg:
         child x              : {Id}
      alternative FSub:
         child x              : {Id}
         child y              : {Id}
      alternative Float:
         child val            : {Float}
      alternative Get:
         child aref           : {Id}
         child iref           : {Id}
      alternative IfEq:
         child x              : {Id}
         child y              : {Id}
         child texpr          : Expr 
         child eexpr          : Expr 
      alternative IfLE:
         child x              : {Id}
         child y              : {Id}
         child texpr          : Expr 
         child eexpr          : Expr 
      alternative Int:
         child val            : {Int}
      alternative Let:
         child tyid           : {TypeId}
         child sub            : Expr 
         child body           : Expr 
      alternative LetRec:
         child fundef         : Fundef 
         child body           : Expr 
      alternative LetTuple:
         child tyids          : {TypeIds}
         child ref            : {Id}
         child body           : Expr 
      alternative Neg:
         child x              : {Id}
      alternative Put:
         child aref           : {Id}
         child iref           : {Id}
         child vref           : {Id}
      alternative Sub:
         child x              : {Id}
         child y              : {Id}
      alternative Tuple:
         child refs           : {[Id]}
      alternative Unit:
      alternative Var:
         child ref            : {Id}
-}
-- cata
sem_Expr :: Expr  ->
            T_Expr 
sem_Expr (Add _x _y )  =
    (sem_Expr_Add _x _y )
sem_Expr (App _ref _args )  =
    (sem_Expr_App _ref _args )
sem_Expr (ExtArray _ref )  =
    (sem_Expr_ExtArray _ref )
sem_Expr (ExtFunApp _nref _args )  =
    (sem_Expr_ExtFunApp _nref _args )
sem_Expr (FAdd _x _y )  =
    (sem_Expr_FAdd _x _y )
sem_Expr (FDiv _x _y )  =
    (sem_Expr_FDiv _x _y )
sem_Expr (FMul _x _y )  =
    (sem_Expr_FMul _x _y )
sem_Expr (FNeg _x )  =
    (sem_Expr_FNeg _x )
sem_Expr (FSub _x _y )  =
    (sem_Expr_FSub _x _y )
sem_Expr (Float _val )  =
    (sem_Expr_Float _val )
sem_Expr (Get _aref _iref )  =
    (sem_Expr_Get _aref _iref )
sem_Expr (IfEq _x _y _texpr _eexpr )  =
    (sem_Expr_IfEq _x _y (sem_Expr _texpr ) (sem_Expr _eexpr ) )
sem_Expr (IfLE _x _y _texpr _eexpr )  =
    (sem_Expr_IfLE _x _y (sem_Expr _texpr ) (sem_Expr _eexpr ) )
sem_Expr (Int _val )  =
    (sem_Expr_Int _val )
sem_Expr (Let _tyid _sub _body )  =
    (sem_Expr_Let _tyid (sem_Expr _sub ) (sem_Expr _body ) )
sem_Expr (LetRec _fundef _body )  =
    (sem_Expr_LetRec (sem_Fundef _fundef ) (sem_Expr _body ) )
sem_Expr (LetTuple _tyids _ref _body )  =
    (sem_Expr_LetTuple _tyids _ref (sem_Expr _body ) )
sem_Expr (Neg _x )  =
    (sem_Expr_Neg _x )
sem_Expr (Put _aref _iref _vref )  =
    (sem_Expr_Put _aref _iref _vref )
sem_Expr (Sub _x _y )  =
    (sem_Expr_Sub _x _y )
sem_Expr (Tuple _refs )  =
    (sem_Expr_Tuple _refs )
sem_Expr (Unit )  =
    (sem_Expr_Unit )
sem_Expr (Var _ref )  =
    (sem_Expr_Var _ref )
-- semantic domain
type T_Expr  = ( )
data Inh_Expr  = Inh_Expr {}
data Syn_Expr  = Syn_Expr {}
wrap_Expr :: T_Expr  ->
             Inh_Expr  ->
             Syn_Expr 
wrap_Expr sem (Inh_Expr )  =
    (let ( ) =
             (sem )
     in  (Syn_Expr ))
sem_Expr_Add :: Id ->
                Id ->
                T_Expr 
sem_Expr_Add x_ y_  =
    (let 
     in  ( ))
sem_Expr_App :: Id ->
                ([Id]) ->
                T_Expr 
sem_Expr_App ref_ args_  =
    (let 
     in  ( ))
sem_Expr_ExtArray :: Id ->
                     T_Expr 
sem_Expr_ExtArray ref_  =
    (let 
     in  ( ))
sem_Expr_ExtFunApp :: Id ->
                      ([Id]) ->
                      T_Expr 
sem_Expr_ExtFunApp nref_ args_  =
    (let 
     in  ( ))
sem_Expr_FAdd :: Id ->
                 Id ->
                 T_Expr 
sem_Expr_FAdd x_ y_  =
    (let 
     in  ( ))
sem_Expr_FDiv :: Id ->
                 Id ->
                 T_Expr 
sem_Expr_FDiv x_ y_  =
    (let 
     in  ( ))
sem_Expr_FMul :: Id ->
                 Id ->
                 T_Expr 
sem_Expr_FMul x_ y_  =
    (let 
     in  ( ))
sem_Expr_FNeg :: Id ->
                 T_Expr 
sem_Expr_FNeg x_  =
    (let 
     in  ( ))
sem_Expr_FSub :: Id ->
                 Id ->
                 T_Expr 
sem_Expr_FSub x_ y_  =
    (let 
     in  ( ))
sem_Expr_Float :: Float ->
                  T_Expr 
sem_Expr_Float val_  =
    (let 
     in  ( ))
sem_Expr_Get :: Id ->
                Id ->
                T_Expr 
sem_Expr_Get aref_ iref_  =
    (let 
     in  ( ))
sem_Expr_IfEq :: Id ->
                 Id ->
                 T_Expr  ->
                 T_Expr  ->
                 T_Expr 
sem_Expr_IfEq x_ y_ texpr_ eexpr_  =
    (let 
     in  ( ))
sem_Expr_IfLE :: Id ->
                 Id ->
                 T_Expr  ->
                 T_Expr  ->
                 T_Expr 
sem_Expr_IfLE x_ y_ texpr_ eexpr_  =
    (let 
     in  ( ))
sem_Expr_Int :: Int ->
                T_Expr 
sem_Expr_Int val_  =
    (let 
     in  ( ))
sem_Expr_Let :: TypeId ->
                T_Expr  ->
                T_Expr  ->
                T_Expr 
sem_Expr_Let tyid_ sub_ body_  =
    (let 
     in  ( ))
sem_Expr_LetRec :: T_Fundef  ->
                   T_Expr  ->
                   T_Expr 
sem_Expr_LetRec fundef_ body_  =
    (let 
     in  ( ))
sem_Expr_LetTuple :: TypeIds ->
                     Id ->
                     T_Expr  ->
                     T_Expr 
sem_Expr_LetTuple tyids_ ref_ body_  =
    (let 
     in  ( ))
sem_Expr_Neg :: Id ->
                T_Expr 
sem_Expr_Neg x_  =
    (let 
     in  ( ))
sem_Expr_Put :: Id ->
                Id ->
                Id ->
                T_Expr 
sem_Expr_Put aref_ iref_ vref_  =
    (let 
     in  ( ))
sem_Expr_Sub :: Id ->
                Id ->
                T_Expr 
sem_Expr_Sub x_ y_  =
    (let 
     in  ( ))
sem_Expr_Tuple :: ([Id]) ->
                  T_Expr 
sem_Expr_Tuple refs_  =
    (let 
     in  ( ))
sem_Expr_Unit :: T_Expr 
sem_Expr_Unit  =
    (let 
     in  ( ))
sem_Expr_Var :: Id ->
                T_Expr 
sem_Expr_Var ref_  =
    (let 
     in  ( ))
-- Fundef ------------------------------------------------------
{-
   alternatives:
      alternative Fundef:
         child name           : {TypeId}
         child args           : {TypeIds}
         child body           : Expr 
-}
-- cata
sem_Fundef :: Fundef  ->
              T_Fundef 
sem_Fundef (Fundef _name _args _body )  =
    (sem_Fundef_Fundef _name _args (sem_Expr _body ) )
-- semantic domain
type T_Fundef  = ( )
data Inh_Fundef  = Inh_Fundef {}
data Syn_Fundef  = Syn_Fundef {}
wrap_Fundef :: T_Fundef  ->
               Inh_Fundef  ->
               Syn_Fundef 
wrap_Fundef sem (Inh_Fundef )  =
    (let ( ) =
             (sem )
     in  (Syn_Fundef ))
sem_Fundef_Fundef :: TypeId ->
                     TypeIds ->
                     T_Expr  ->
                     T_Fundef 
sem_Fundef_Fundef name_ args_ body_  =
    (let 
     in  ( ))