

-- UUAGC 0.9.6 (Closure.ag)


-- |
-- Module: HMinCaml.Closure
-- License: as per original MinCaml
--
-- Maintainer: Stephen Tetley <stephen.tetley@gmail.com>
-- Stability: unstable
-- Portability: ghc
--
-- Closure conversion
--


module HMinCaml.Closure where

import HMinCaml.ClosureSyn
import HMinCaml.Id
import qualified HMinCaml.KNormalSyn as K
import qualified HMinCaml.M as M
import qualified HMinCaml.S as S
import HMinCaml.Type




freeVars :: Expr -> S.S Id
freeVars expr = fv_Syn_Expr synthesized
  where
    synthesized = wrap_Expr (sem_Expr expr) inherited
    inherited = Inh_Expr { }
    

-- Closure -----------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         fv                   : S.S Id
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
type T_Closure  = ( (S.S Id))
data Inh_Closure  = Inh_Closure {}
data Syn_Closure  = Syn_Closure {fv_Syn_Closure :: S.S Id}
wrap_Closure :: T_Closure  ->
                Inh_Closure  ->
                Syn_Closure 
wrap_Closure sem (Inh_Closure )  =
    (let ( _lhsOfv) =
             (sem )
     in  (Syn_Closure _lhsOfv ))
sem_Closure_Closure :: Label ->
                       ([Id]) ->
                       T_Closure 
sem_Closure_Closure entry_ actual_fv_  =
    (let _lhsOfv :: (S.S Id)
         -- "Closure.ag"(line 62, column 15)
         _lhsOfv =
             S.ofList actual_fv_
     in  ( _lhsOfv))
-- Expr --------------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         fv                   : S.S Id
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
         child tyid           : TypeId 
         child sub            : Expr 
         child body           : Expr 
      alternative LetTuple:
         child tyids          : TypeIds 
         child ref            : {Id}
         child body           : Expr 
      alternative MakeCls:
         child tyid           : TypeId 
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
    (sem_Expr_Let (sem_TypeId _tyid ) (sem_Expr _sub ) (sem_Expr _body ) )
sem_Expr (LetTuple _tyids _ref _body )  =
    (sem_Expr_LetTuple (sem_TypeIds _tyids ) _ref (sem_Expr _body ) )
sem_Expr (MakeCls _tyid _close _body )  =
    (sem_Expr_MakeCls (sem_TypeId _tyid ) (sem_Closure _close ) (sem_Expr _body ) )
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
type T_Expr  = ( (S.S Id))
data Inh_Expr  = Inh_Expr {}
data Syn_Expr  = Syn_Expr {fv_Syn_Expr :: S.S Id}
wrap_Expr :: T_Expr  ->
             Inh_Expr  ->
             Syn_Expr 
wrap_Expr sem (Inh_Expr )  =
    (let ( _lhsOfv) =
             (sem )
     in  (Syn_Expr _lhsOfv ))
sem_Expr_Add :: Id ->
                Id ->
                T_Expr 
sem_Expr_Add x_ y_  =
    (let _lhsOfv :: (S.S Id)
         -- "Closure.ag"(line 41, column 15)
         _lhsOfv =
             S.ofList [x_, y_]
     in  ( _lhsOfv))
sem_Expr_AppCls :: Id ->
                   ([Id]) ->
                   T_Expr 
sem_Expr_AppCls ref_ args_  =
    (let _lhsOfv :: (S.S Id)
         -- "Closure.ag"(line 54, column 15)
         _lhsOfv =
             S.ofList (ref_ : args_)
     in  ( _lhsOfv))
sem_Expr_AppDir :: Label ->
                   ([Id]) ->
                   T_Expr 
sem_Expr_AppDir lbl_ args_  =
    (let _lhsOfv :: (S.S Id)
         -- "Closure.ag"(line 55, column 15)
         _lhsOfv =
             S.ofList args_
     in  ( _lhsOfv))
sem_Expr_ExtArray :: Label ->
                     T_Expr 
sem_Expr_ExtArray lbl_  =
    (let _lhsOfv :: (S.S Id)
         -- "Closure.ag"(line 38, column 15)
         _lhsOfv =
             S.empty
     in  ( _lhsOfv))
sem_Expr_FAdd :: Id ->
                 Id ->
                 T_Expr 
sem_Expr_FAdd x_ y_  =
    (let _lhsOfv :: (S.S Id)
         -- "Closure.ag"(line 43, column 15)
         _lhsOfv =
             S.ofList [x_, y_]
     in  ( _lhsOfv))
sem_Expr_FDiv :: Id ->
                 Id ->
                 T_Expr 
sem_Expr_FDiv x_ y_  =
    (let _lhsOfv :: (S.S Id)
         -- "Closure.ag"(line 46, column 15)
         _lhsOfv =
             S.ofList [x_, y_]
     in  ( _lhsOfv))
sem_Expr_FMul :: Id ->
                 Id ->
                 T_Expr 
sem_Expr_FMul x_ y_  =
    (let _lhsOfv :: (S.S Id)
         -- "Closure.ag"(line 45, column 15)
         _lhsOfv =
             S.ofList [x_, y_]
     in  ( _lhsOfv))
sem_Expr_FNeg :: Id ->
                 T_Expr 
sem_Expr_FNeg x_  =
    (let _lhsOfv :: (S.S Id)
         -- "Closure.ag"(line 40, column 15)
         _lhsOfv =
             S.singleton x_
     in  ( _lhsOfv))
sem_Expr_FSub :: Id ->
                 Id ->
                 T_Expr 
sem_Expr_FSub x_ y_  =
    (let _lhsOfv :: (S.S Id)
         -- "Closure.ag"(line 44, column 15)
         _lhsOfv =
             S.ofList [x_, y_]
     in  ( _lhsOfv))
sem_Expr_Float :: Float ->
                  T_Expr 
sem_Expr_Float val_  =
    (let _lhsOfv :: (S.S Id)
         -- "Closure.ag"(line 37, column 15)
         _lhsOfv =
             S.empty
     in  ( _lhsOfv))
sem_Expr_Get :: Id ->
                Id ->
                T_Expr 
sem_Expr_Get aref_ iref_  =
    (let _lhsOfv :: (S.S Id)
         -- "Closure.ag"(line 47, column 15)
         _lhsOfv =
             S.ofList [aref_, iref_]
     in  ( _lhsOfv))
sem_Expr_IfEq :: Id ->
                 Id ->
                 T_Expr  ->
                 T_Expr  ->
                 T_Expr 
sem_Expr_IfEq x_ y_ texpr_ eexpr_  =
    (let _lhsOfv :: (S.S Id)
         _texprIfv :: (S.S Id)
         _eexprIfv :: (S.S Id)
         -- "Closure.ag"(line 48, column 15)
         _lhsOfv =
             S.add x_ (S.add y_ (S.union _texprIfv _eexprIfv))
         ( _texprIfv) =
             (texpr_ )
         ( _eexprIfv) =
             (eexpr_ )
     in  ( _lhsOfv))
sem_Expr_IfLE :: Id ->
                 Id ->
                 T_Expr  ->
                 T_Expr  ->
                 T_Expr 
sem_Expr_IfLE x_ y_ texpr_ eexpr_  =
    (let _lhsOfv :: (S.S Id)
         _texprIfv :: (S.S Id)
         _eexprIfv :: (S.S Id)
         -- "Closure.ag"(line 49, column 15)
         _lhsOfv =
             S.add x_ (S.add y_ (S.union _texprIfv _eexprIfv))
         ( _texprIfv) =
             (texpr_ )
         ( _eexprIfv) =
             (eexpr_ )
     in  ( _lhsOfv))
sem_Expr_Int :: Int ->
                T_Expr 
sem_Expr_Int val_  =
    (let _lhsOfv :: (S.S Id)
         -- "Closure.ag"(line 36, column 15)
         _lhsOfv =
             S.empty
     in  ( _lhsOfv))
sem_Expr_Let :: T_TypeId  ->
                T_Expr  ->
                T_Expr  ->
                T_Expr 
sem_Expr_Let tyid_ sub_ body_  =
    (let _lhsOfv :: (S.S Id)
         _tyidIargId :: Id
         _tyidIcopy :: TypeId
         _subIfv :: (S.S Id)
         _bodyIfv :: (S.S Id)
         -- "Closure.ag"(line 50, column 15)
         _lhsOfv =
             S.union _subIfv (S.remove _tyidIargId _bodyIfv)
         ( _tyidIargId,_tyidIcopy) =
             (tyid_ )
         ( _subIfv) =
             (sub_ )
         ( _bodyIfv) =
             (body_ )
     in  ( _lhsOfv))
sem_Expr_LetTuple :: T_TypeIds  ->
                     Id ->
                     T_Expr  ->
                     T_Expr 
sem_Expr_LetTuple tyids_ ref_ body_  =
    (let _lhsOfv :: (S.S Id)
         _tyidsIargId :: ([Id])
         _tyidsIcopy :: TypeIds
         _bodyIfv :: (S.S Id)
         -- "Closure.ag"(line 57, column 15)
         _lhsOfv =
             S.add ref_ (S.diff _bodyIfv
                                (S.ofList _tyidsIargId))
         ( _tyidsIargId,_tyidsIcopy) =
             (tyids_ )
         ( _bodyIfv) =
             (body_ )
     in  ( _lhsOfv))
sem_Expr_MakeCls :: T_TypeId  ->
                    T_Closure  ->
                    T_Expr  ->
                    T_Expr 
sem_Expr_MakeCls tyid_ close_ body_  =
    (let _lhsOfv :: (S.S Id)
         _tyidIargId :: Id
         _tyidIcopy :: TypeId
         _closeIfv :: (S.S Id)
         _bodyIfv :: (S.S Id)
         -- "Closure.ag"(line 52, column 15)
         _lhsOfv =
             S.remove _tyidIargId
                      (S.union _closeIfv _bodyIfv)
         ( _tyidIargId,_tyidIcopy) =
             (tyid_ )
         ( _closeIfv) =
             (close_ )
         ( _bodyIfv) =
             (body_ )
     in  ( _lhsOfv))
sem_Expr_Neg :: Id ->
                T_Expr 
sem_Expr_Neg x_  =
    (let _lhsOfv :: (S.S Id)
         -- "Closure.ag"(line 39, column 15)
         _lhsOfv =
             S.singleton x_
     in  ( _lhsOfv))
sem_Expr_Put :: Id ->
                Id ->
                Id ->
                T_Expr 
sem_Expr_Put aref_ iref_ vref_  =
    (let _lhsOfv :: (S.S Id)
         -- "Closure.ag"(line 59, column 15)
         _lhsOfv =
             S.ofList [aref_, iref_, vref_]
     in  ( _lhsOfv))
sem_Expr_Sub :: Id ->
                Id ->
                T_Expr 
sem_Expr_Sub x_ y_  =
    (let _lhsOfv :: (S.S Id)
         -- "Closure.ag"(line 42, column 15)
         _lhsOfv =
             S.ofList [x_, y_]
     in  ( _lhsOfv))
sem_Expr_Tuple :: ([Id]) ->
                  T_Expr 
sem_Expr_Tuple refs_  =
    (let _lhsOfv :: (S.S Id)
         -- "Closure.ag"(line 56, column 15)
         _lhsOfv =
             S.ofList refs_
     in  ( _lhsOfv))
sem_Expr_Unit :: T_Expr 
sem_Expr_Unit  =
    (let _lhsOfv :: (S.S Id)
         -- "Closure.ag"(line 35, column 15)
         _lhsOfv =
             S.empty
     in  ( _lhsOfv))
sem_Expr_Var :: Id ->
                T_Expr 
sem_Expr_Var x_  =
    (let _lhsOfv :: (S.S Id)
         -- "Closure.ag"(line 51, column 15)
         _lhsOfv =
             S.singleton x_
     in  ( _lhsOfv))
-- Fundef ------------------------------------------------------
{-
   alternatives:
      alternative Fundef:
         child name           : LabeledType 
         child args           : TypeIds 
         child formal_fv      : TypeIds 
         child body           : Expr 
-}
-- cata
sem_Fundef :: Fundef  ->
              T_Fundef 
sem_Fundef (Fundef _name _args _formal_fv _body )  =
    (sem_Fundef_Fundef (sem_LabeledType _name ) (sem_TypeIds _args ) (sem_TypeIds _formal_fv ) (sem_Expr _body ) )
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
sem_Fundef_Fundef :: T_LabeledType  ->
                     T_TypeIds  ->
                     T_TypeIds  ->
                     T_Expr  ->
                     T_Fundef 
sem_Fundef_Fundef name_ args_ formal_fv_ body_  =
    (let _argsIargId :: ([Id])
         _argsIcopy :: TypeIds
         _formal_fvIargId :: ([Id])
         _formal_fvIcopy :: TypeIds
         _bodyIfv :: (S.S Id)
         ( _argsIargId,_argsIcopy) =
             (args_ )
         ( _formal_fvIargId,_formal_fvIcopy) =
             (formal_fv_ )
         ( _bodyIfv) =
             (body_ )
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
-- LabeledType -------------------------------------------------
{-
   alternatives:
      alternative Tuple:
         child argId          : {Label}
         child argType        : Type 
-}
-- cata
sem_LabeledType :: LabeledType  ->
                   T_LabeledType 
sem_LabeledType ( argId,argType)  =
    (sem_LabeledType_Tuple argId (sem_Type argType ) )
-- semantic domain
type T_LabeledType  = ( )
data Inh_LabeledType  = Inh_LabeledType {}
data Syn_LabeledType  = Syn_LabeledType {}
wrap_LabeledType :: T_LabeledType  ->
                    Inh_LabeledType  ->
                    Syn_LabeledType 
wrap_LabeledType sem (Inh_LabeledType )  =
    (let ( ) =
             (sem )
     in  (Syn_LabeledType ))
sem_LabeledType_Tuple :: Label ->
                         T_Type  ->
                         T_LabeledType 
sem_LabeledType_Tuple argId_ argType_  =
    (let _argTypeIcopy :: Type
         ( _argTypeIcopy) =
             (argType_ )
     in  ( ))
-- OptType -----------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         copy                 : SELF 
   alternatives:
      alternative Just:
         child just           : Type 
         visit 0:
            local copy        : _
      alternative Nothing:
         visit 0:
            local copy        : _
-}
-- cata
sem_OptType :: OptType  ->
               T_OptType 
sem_OptType (Prelude.Just x )  =
    (sem_OptType_Just (sem_Type x ) )
sem_OptType Prelude.Nothing  =
    sem_OptType_Nothing
-- semantic domain
type T_OptType  = ( OptType)
data Inh_OptType  = Inh_OptType {}
data Syn_OptType  = Syn_OptType {copy_Syn_OptType :: OptType}
wrap_OptType :: T_OptType  ->
                Inh_OptType  ->
                Syn_OptType 
wrap_OptType sem (Inh_OptType )  =
    (let ( _lhsOcopy) =
             (sem )
     in  (Syn_OptType _lhsOcopy ))
sem_OptType_Just :: T_Type  ->
                    T_OptType 
sem_OptType_Just just_  =
    (let _lhsOcopy :: OptType
         _justIcopy :: Type
         -- self rule
         _copy =
             Just _justIcopy
         -- self rule
         _lhsOcopy =
             _copy
         ( _justIcopy) =
             (just_ )
     in  ( _lhsOcopy))
sem_OptType_Nothing :: T_OptType 
sem_OptType_Nothing  =
    (let _lhsOcopy :: OptType
         -- self rule
         _copy =
             Nothing
         -- self rule
         _lhsOcopy =
             _copy
     in  ( _lhsOcopy))
-- Prog --------------------------------------------------------
{-
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
type T_Prog  = ( )
data Inh_Prog  = Inh_Prog {}
data Syn_Prog  = Syn_Prog {}
wrap_Prog :: T_Prog  ->
             Inh_Prog  ->
             Syn_Prog 
wrap_Prog sem (Inh_Prog )  =
    (let ( ) =
             (sem )
     in  (Syn_Prog ))
sem_Prog_Prog :: T_Fundefs  ->
                 T_Expr  ->
                 T_Prog 
sem_Prog_Prog defs_ body_  =
    (let _bodyIfv :: (S.S Id)
         ( _bodyIfv) =
             (body_ )
     in  ( ))
-- Type --------------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         copy                 : SELF 
   alternatives:
      alternative TArray:
         child ty             : Type 
         visit 0:
            local copy        : _
      alternative TBool:
         visit 0:
            local copy        : _
      alternative TFloat:
         visit 0:
            local copy        : _
      alternative TFun:
         child args           : Types 
         child retTy          : Type 
         visit 0:
            local copy        : _
      alternative TInt:
         visit 0:
            local copy        : _
      alternative TTuple:
         child tys            : Types 
         visit 0:
            local copy        : _
      alternative TUnit:
         visit 0:
            local copy        : _
      alternative TVar:
         child optTy          : OptType 
         visit 0:
            local copy        : _
-}
-- cata
sem_Type :: Type  ->
            T_Type 
sem_Type (TArray _ty )  =
    (sem_Type_TArray (sem_Type _ty ) )
sem_Type (TBool )  =
    (sem_Type_TBool )
sem_Type (TFloat )  =
    (sem_Type_TFloat )
sem_Type (TFun _args _retTy )  =
    (sem_Type_TFun (sem_Types _args ) (sem_Type _retTy ) )
sem_Type (TInt )  =
    (sem_Type_TInt )
sem_Type (TTuple _tys )  =
    (sem_Type_TTuple (sem_Types _tys ) )
sem_Type (TUnit )  =
    (sem_Type_TUnit )
sem_Type (TVar _optTy )  =
    (sem_Type_TVar (sem_OptType _optTy ) )
-- semantic domain
type T_Type  = ( Type)
data Inh_Type  = Inh_Type {}
data Syn_Type  = Syn_Type {copy_Syn_Type :: Type}
wrap_Type :: T_Type  ->
             Inh_Type  ->
             Syn_Type 
wrap_Type sem (Inh_Type )  =
    (let ( _lhsOcopy) =
             (sem )
     in  (Syn_Type _lhsOcopy ))
sem_Type_TArray :: T_Type  ->
                   T_Type 
sem_Type_TArray ty_  =
    (let _lhsOcopy :: Type
         _tyIcopy :: Type
         -- self rule
         _copy =
             TArray _tyIcopy
         -- self rule
         _lhsOcopy =
             _copy
         ( _tyIcopy) =
             (ty_ )
     in  ( _lhsOcopy))
sem_Type_TBool :: T_Type 
sem_Type_TBool  =
    (let _lhsOcopy :: Type
         -- self rule
         _copy =
             TBool
         -- self rule
         _lhsOcopy =
             _copy
     in  ( _lhsOcopy))
sem_Type_TFloat :: T_Type 
sem_Type_TFloat  =
    (let _lhsOcopy :: Type
         -- self rule
         _copy =
             TFloat
         -- self rule
         _lhsOcopy =
             _copy
     in  ( _lhsOcopy))
sem_Type_TFun :: T_Types  ->
                 T_Type  ->
                 T_Type 
sem_Type_TFun args_ retTy_  =
    (let _lhsOcopy :: Type
         _argsIcopy :: Types
         _retTyIcopy :: Type
         -- self rule
         _copy =
             TFun _argsIcopy _retTyIcopy
         -- self rule
         _lhsOcopy =
             _copy
         ( _argsIcopy) =
             (args_ )
         ( _retTyIcopy) =
             (retTy_ )
     in  ( _lhsOcopy))
sem_Type_TInt :: T_Type 
sem_Type_TInt  =
    (let _lhsOcopy :: Type
         -- self rule
         _copy =
             TInt
         -- self rule
         _lhsOcopy =
             _copy
     in  ( _lhsOcopy))
sem_Type_TTuple :: T_Types  ->
                   T_Type 
sem_Type_TTuple tys_  =
    (let _lhsOcopy :: Type
         _tysIcopy :: Types
         -- self rule
         _copy =
             TTuple _tysIcopy
         -- self rule
         _lhsOcopy =
             _copy
         ( _tysIcopy) =
             (tys_ )
     in  ( _lhsOcopy))
sem_Type_TUnit :: T_Type 
sem_Type_TUnit  =
    (let _lhsOcopy :: Type
         -- self rule
         _copy =
             TUnit
         -- self rule
         _lhsOcopy =
             _copy
     in  ( _lhsOcopy))
sem_Type_TVar :: T_OptType  ->
                 T_Type 
sem_Type_TVar optTy_  =
    (let _lhsOcopy :: Type
         _optTyIcopy :: OptType
         -- self rule
         _copy =
             TVar _optTyIcopy
         -- self rule
         _lhsOcopy =
             _copy
         ( _optTyIcopy) =
             (optTy_ )
     in  ( _lhsOcopy))
-- TypeId ------------------------------------------------------
{-
   visit 0:
      synthesized attributes:
         argId                : Id
         copy                 : SELF 
   alternatives:
      alternative Tuple:
         child argId          : {Id}
         child argType        : Type 
         visit 0:
            local copy        : _
-}
-- cata
sem_TypeId :: TypeId  ->
              T_TypeId 
sem_TypeId ( argId,argType)  =
    (sem_TypeId_Tuple argId (sem_Type argType ) )
-- semantic domain
type T_TypeId  = ( Id,TypeId)
data Inh_TypeId  = Inh_TypeId {}
data Syn_TypeId  = Syn_TypeId {argId_Syn_TypeId :: Id,copy_Syn_TypeId :: TypeId}
wrap_TypeId :: T_TypeId  ->
               Inh_TypeId  ->
               Syn_TypeId 
wrap_TypeId sem (Inh_TypeId )  =
    (let ( _lhsOargId,_lhsOcopy) =
             (sem )
     in  (Syn_TypeId _lhsOargId _lhsOcopy ))
sem_TypeId_Tuple :: Id ->
                    T_Type  ->
                    T_TypeId 
sem_TypeId_Tuple argId_ argType_  =
    (let _lhsOargId :: Id
         _lhsOcopy :: TypeId
         _argTypeIcopy :: Type
         -- "./TypeDEFS.ag"(line 32, column 15)
         _lhsOargId =
             argId_
         -- self rule
         _copy =
             (argId_,_argTypeIcopy)
         -- self rule
         _lhsOcopy =
             _copy
         ( _argTypeIcopy) =
             (argType_ )
     in  ( _lhsOargId,_lhsOcopy))
-- TypeIds -----------------------------------------------------
{-
   visit 0:
      synthesized attributes:
         argId                : [Id]
         copy                 : SELF 
   alternatives:
      alternative Cons:
         child hd             : TypeId 
         child tl             : TypeIds 
         visit 0:
            local copy        : _
      alternative Nil:
         visit 0:
            local copy        : _
-}
-- cata
sem_TypeIds :: TypeIds  ->
               T_TypeIds 
sem_TypeIds list  =
    (Prelude.foldr sem_TypeIds_Cons sem_TypeIds_Nil (Prelude.map sem_TypeId list) )
-- semantic domain
type T_TypeIds  = ( ([Id]),TypeIds)
data Inh_TypeIds  = Inh_TypeIds {}
data Syn_TypeIds  = Syn_TypeIds {argId_Syn_TypeIds :: [Id],copy_Syn_TypeIds :: TypeIds}
wrap_TypeIds :: T_TypeIds  ->
                Inh_TypeIds  ->
                Syn_TypeIds 
wrap_TypeIds sem (Inh_TypeIds )  =
    (let ( _lhsOargId,_lhsOcopy) =
             (sem )
     in  (Syn_TypeIds _lhsOargId _lhsOcopy ))
sem_TypeIds_Cons :: T_TypeId  ->
                    T_TypeIds  ->
                    T_TypeIds 
sem_TypeIds_Cons hd_ tl_  =
    (let _lhsOargId :: ([Id])
         _lhsOcopy :: TypeIds
         _hdIargId :: Id
         _hdIcopy :: TypeId
         _tlIargId :: ([Id])
         _tlIcopy :: TypeIds
         -- use rule "./TypeDEFS.ag"(line 28, column 32)
         _lhsOargId =
             _hdIargId : _tlIargId
         -- self rule
         _copy =
             (:) _hdIcopy _tlIcopy
         -- self rule
         _lhsOcopy =
             _copy
         ( _hdIargId,_hdIcopy) =
             (hd_ )
         ( _tlIargId,_tlIcopy) =
             (tl_ )
     in  ( _lhsOargId,_lhsOcopy))
sem_TypeIds_Nil :: T_TypeIds 
sem_TypeIds_Nil  =
    (let _lhsOargId :: ([Id])
         _lhsOcopy :: TypeIds
         -- use rule "./TypeDEFS.ag"(line 28, column 32)
         _lhsOargId =
             []
         -- self rule
         _copy =
             []
         -- self rule
         _lhsOcopy =
             _copy
     in  ( _lhsOargId,_lhsOcopy))
-- Types -------------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         copy                 : SELF 
   alternatives:
      alternative Cons:
         child hd             : Type 
         child tl             : Types 
         visit 0:
            local copy        : _
      alternative Nil:
         visit 0:
            local copy        : _
-}
-- cata
sem_Types :: Types  ->
             T_Types 
sem_Types list  =
    (Prelude.foldr sem_Types_Cons sem_Types_Nil (Prelude.map sem_Type list) )
-- semantic domain
type T_Types  = ( Types)
data Inh_Types  = Inh_Types {}
data Syn_Types  = Syn_Types {copy_Syn_Types :: Types}
wrap_Types :: T_Types  ->
              Inh_Types  ->
              Syn_Types 
wrap_Types sem (Inh_Types )  =
    (let ( _lhsOcopy) =
             (sem )
     in  (Syn_Types _lhsOcopy ))
sem_Types_Cons :: T_Type  ->
                  T_Types  ->
                  T_Types 
sem_Types_Cons hd_ tl_  =
    (let _lhsOcopy :: Types
         _hdIcopy :: Type
         _tlIcopy :: Types
         -- self rule
         _copy =
             (:) _hdIcopy _tlIcopy
         -- self rule
         _lhsOcopy =
             _copy
         ( _hdIcopy) =
             (hd_ )
         ( _tlIcopy) =
             (tl_ )
     in  ( _lhsOcopy))
sem_Types_Nil :: T_Types 
sem_Types_Nil  =
    (let _lhsOcopy :: Types
         -- self rule
         _copy =
             []
         -- self rule
         _lhsOcopy =
             _copy
     in  ( _lhsOcopy))