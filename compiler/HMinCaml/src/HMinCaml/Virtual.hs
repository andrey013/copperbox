

-- UUAGC 0.9.6 (Virtual.ag)


-- |
-- Module: HMinCaml.Virtual
-- License: as per original MinCaml
--
-- Maintainer: Stephen Tetley <stephen.tetley@gmail.com>
-- Stability: unstable
-- Portability: ghc
--
-- Translate to Sparc assembly with infinite registers
--


module HMinCaml.Virtual where

import HMinCaml.ClosureSyn
import HMinCaml.CompilerMonad
import HMinCaml.Id
import qualified HMinCaml.M as M
import HMinCaml.SparcAsm
import qualified HMinCaml.SparcAsmSyn as S
import HMinCaml.Type

import Data.List ( foldl' )



classify xts ini addf addi = foldl' fn ini xts where
    fn acc (_,TUnit)    = acc
    fn acc (x,TFloat)   = addf acc x
    fn acc (x,t)        = addi acc x t

separate xts = classify xts ([], []) ff fi where
    ff (int,float) x    = (int, float ++ [x])
    fi (int,float) x _  = (int ++ [x], float)

expand xts ini addf addi = classify xts ini ff fi where
    ff (offset,acc) x     = let offset' = align offset
                            in (offset' + 8, addf x offset' acc)
    fi (offset, acc) x t  = (offset + 4, addi (x,t) offset acc)




virtual :: Prog -> S.Prog
virtual prog = virtual_Syn_Prog synthesized
  where
    synthesized = wrap_Prog (sem_Prog prog) inherited
    inherited   = Inh_Prog { env_Inh_Prog = M.empty }
    

-- Closure -----------------------------------------------------
{-
   alternatives:
      alternative Closure:
         child entry          : {Label}
         child actual_fv      : {[Id]}
-}
-- cata
sem_Closure :: Closure  ->
               T_Closure 
sem_Closure (Closure _entry _actual_fv )  =
    (sem_Closure_Closure _entry _actual_fv )
-- semantic domain
type T_Closure  = ( )
data Inh_Closure  = Inh_Closure {}
data Syn_Closure  = Syn_Closure {}
wrap_Closure :: T_Closure  ->
                Inh_Closure  ->
                Syn_Closure 
wrap_Closure sem (Inh_Closure )  =
    (let ( ) =
             (sem )
     in  (Syn_Closure ))
sem_Closure_Closure :: Label ->
                       ([Id]) ->
                       T_Closure 
sem_Closure_Closure entry_ actual_fv_  =
    (let 
     in  ( ))
-- Expr --------------------------------------------------------
{-
   alternatives:
      alternative Add:
         child x              : {Id}
         child y              : {Id}
      alternative AppCls:
         child ref            : {Id}
         child args           : {[Id]}
      alternative AppDir:
         child lbl            : {Label}
         child args           : {[Id]}
      alternative ExtArray:
         child lbl            : {Label}
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
      alternative LetTuple:
         child tyids          : {TypeIds}
         child ref            : {Id}
         child body           : Expr 
      alternative MakeCls:
         child tyid           : {TypeId}
         child close          : Closure 
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
         child x              : {Id}
-}
-- cata
sem_Expr :: Expr  ->
            T_Expr 
sem_Expr (Add _x _y )  =
    (sem_Expr_Add _x _y )
sem_Expr (AppCls _ref _args )  =
    (sem_Expr_AppCls _ref _args )
sem_Expr (AppDir _lbl _args )  =
    (sem_Expr_AppDir _lbl _args )
sem_Expr (ExtArray _lbl )  =
    (sem_Expr_ExtArray _lbl )
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
sem_Expr (LetTuple _tyids _ref _body )  =
    (sem_Expr_LetTuple _tyids _ref (sem_Expr _body ) )
sem_Expr (MakeCls _tyid _close _body )  =
    (sem_Expr_MakeCls _tyid (sem_Closure _close ) (sem_Expr _body ) )
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
sem_Expr (Var _x )  =
    (sem_Expr_Var _x )
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
sem_Expr_AppCls :: Id ->
                   ([Id]) ->
                   T_Expr 
sem_Expr_AppCls ref_ args_  =
    (let 
     in  ( ))
sem_Expr_AppDir :: Label ->
                   ([Id]) ->
                   T_Expr 
sem_Expr_AppDir lbl_ args_  =
    (let 
     in  ( ))
sem_Expr_ExtArray :: Label ->
                     T_Expr 
sem_Expr_ExtArray lbl_  =
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
sem_Expr_LetTuple :: TypeIds ->
                     Id ->
                     T_Expr  ->
                     T_Expr 
sem_Expr_LetTuple tyids_ ref_ body_  =
    (let 
     in  ( ))
sem_Expr_MakeCls :: TypeId ->
                    T_Closure  ->
                    T_Expr  ->
                    T_Expr 
sem_Expr_MakeCls tyid_ close_ body_  =
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
sem_Expr_Var x_  =
    (let 
     in  ( ))
-- Fundef ------------------------------------------------------
{-
   alternatives:
      alternative Fundef:
         child name           : {LabeledType}
         child args           : {TypeIds}
         child formal_fv      : {TypeIds}
         child body           : Expr 
-}
-- cata
sem_Fundef :: Fundef  ->
              T_Fundef 
sem_Fundef (Fundef _name _args _formal_fv _body )  =
    (sem_Fundef_Fundef _name _args _formal_fv (sem_Expr _body ) )
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
sem_Fundef_Fundef :: LabeledType ->
                     TypeIds ->
                     TypeIds ->
                     T_Expr  ->
                     T_Fundef 
sem_Fundef_Fundef name_ args_ formal_fv_ body_  =
    (let 
     in  ( ))
-- Fundefs -----------------------------------------------------
{-
   alternatives:
      alternative Cons:
         child hd             : Fundef 
         child tl             : Fundefs 
      alternative Nil:
-}
-- cata
sem_Fundefs :: Fundefs  ->
               T_Fundefs 
sem_Fundefs list  =
    (Prelude.foldr sem_Fundefs_Cons sem_Fundefs_Nil (Prelude.map sem_Fundef list) )
-- semantic domain
type T_Fundefs  = ( )
data Inh_Fundefs  = Inh_Fundefs {}
data Syn_Fundefs  = Syn_Fundefs {}
wrap_Fundefs :: T_Fundefs  ->
                Inh_Fundefs  ->
                Syn_Fundefs 
wrap_Fundefs sem (Inh_Fundefs )  =
    (let ( ) =
             (sem )
     in  (Syn_Fundefs ))
sem_Fundefs_Cons :: T_Fundef  ->
                    T_Fundefs  ->
                    T_Fundefs 
sem_Fundefs_Cons hd_ tl_  =
    (let 
     in  ( ))
sem_Fundefs_Nil :: T_Fundefs 
sem_Fundefs_Nil  =
    (let 
     in  ( ))
-- Prog --------------------------------------------------------
{-
   visit 0:
      chained attribute:
         env                  : M.M Id Int
      synthesized attribute:
         virtual              : S.Prog
   alternatives:
      alternative Prog:
         child defs           : Fundefs 
         child body           : Expr 
-}
-- cata
sem_Prog :: Prog  ->
            T_Prog 
sem_Prog (Prog _defs _body )  =
    (sem_Prog_Prog (sem_Fundefs _defs ) (sem_Expr _body ) )
-- semantic domain
type T_Prog  = (M.M Id Int) ->
               ( (M.M Id Int),(S.Prog))
data Inh_Prog  = Inh_Prog {env_Inh_Prog :: M.M Id Int}
data Syn_Prog  = Syn_Prog {env_Syn_Prog :: M.M Id Int,virtual_Syn_Prog :: S.Prog}
wrap_Prog :: T_Prog  ->
             Inh_Prog  ->
             Syn_Prog 
wrap_Prog sem (Inh_Prog _lhsIenv )  =
    (let ( _lhsOenv,_lhsOvirtual) =
             (sem _lhsIenv )
     in  (Syn_Prog _lhsOenv _lhsOvirtual ))
sem_Prog_Prog :: T_Fundefs  ->
                 T_Expr  ->
                 T_Prog 
sem_Prog_Prog defs_ body_  =
    (\ _lhsIenv ->
         (let _lhsOvirtual :: (S.Prog)
              _lhsOenv :: (M.M Id Int)
              -- "Virtual.ag"(line 35, column 17)
              _lhsOvirtual =
                  undefined
              -- copy rule (chain)
              _lhsOenv =
                  _lhsIenv
          in  ( _lhsOenv,_lhsOvirtual)))