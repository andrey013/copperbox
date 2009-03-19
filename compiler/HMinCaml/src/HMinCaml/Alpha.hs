

-- UUAGC 0.9.6 (Alpha.ag)


-- |
-- Module: HMinCaml.Alpha
-- License: as per original MinCaml
--
-- Maintainer: Stephen Tetley <stephen.tetley@gmail.com>
-- Stability: unstable
-- Portability: ghc
--
-- Alpha conversion
--



module HMinCaml.Alpha where


import HMinCaml.Id
import qualified HMinCaml.M as M
import HMinCaml.KNormal ( Expr(..), Fundef(..) )
import HMinCaml.Type
import HMinCaml.Utils ( (&?), (&+) )



type Env = M.M Id Id






alpha :: Expr -> Expr
alpha expr = alpha_Syn_Expr synthesized
  where
    synthesized = wrap_Expr (sem_Expr expr) inherited
    inherited = Inh_Expr { env_Inh_Expr = M.empty }

-- Expr --------------------------------------------------------
{-
   visit 0:
      chained attributes:
         env                  : Env
         fresh                : Int
      synthesized attribute:
         alpha                : Expr
   alternatives:
      alternative Add:
         child ref1           : {Id}
         child ref2           : {Id}
      alternative App:
         child ref            : {Id}
         child args           : {[Id]}
      alternative ExtArray:
         child ref            : {Id}
      alternative ExtFunApp:
         child nref           : {Id}
         child args           : {[Id]}
      alternative FAdd:
         child ref1           : {Id}
         child ref2           : {Id}
      alternative FDiv:
         child ref1           : {Id}
         child ref2           : {Id}
      alternative FMul:
         child ref1           : {Id}
         child ref2           : {Id}
      alternative FNeg:
         child ref            : {Id}
      alternative FSub:
         child ref1           : {Id}
         child ref2           : {Id}
      alternative Float:
         child val            : {Float}
      alternative Get:
         child aref           : {Id}
         child iref           : {Id}
      alternative IfEq:
         child ref1           : {Id}
         child ref2           : {Id}
         child texpr          : Expr 
         child eexpr          : Expr 
      alternative IfLE:
         child ref1           : {Id}
         child ref2           : {Id}
         child texpr          : Expr 
         child eexpr          : Expr 
      alternative Int:
         child val            : {Int}
      alternative Let:
         child tyid           : TypeId 
         child sub            : Expr 
         child body           : Expr 
         visit 0:
            local newlab      : _
      alternative LetRec:
         child fundef         : Fundef 
         child body           : Expr 
      alternative LetTuple:
         child tyids          : TypeIds 
         child ref            : {Id}
         child body           : Expr 
         visit 0:
            local env'        : _
      alternative Neg:
         child ref            : {Id}
      alternative Put:
         child aref           : {Id}
         child iref           : {Id}
         child vref           : {Id}
      alternative Sub:
         child ref1           : {Id}
         child ref2           : {Id}
      alternative Tuple:
         child refs           : {[Id]}
      alternative Unit:
      alternative Var:
         child ref            : {Id}
-}
-- cata
sem_Expr :: Expr  ->
            T_Expr 
sem_Expr (Add _ref1 _ref2 )  =
    (sem_Expr_Add _ref1 _ref2 )
sem_Expr (App _ref _args )  =
    (sem_Expr_App _ref _args )
sem_Expr (ExtArray _ref )  =
    (sem_Expr_ExtArray _ref )
sem_Expr (ExtFunApp _nref _args )  =
    (sem_Expr_ExtFunApp _nref _args )
sem_Expr (FAdd _ref1 _ref2 )  =
    (sem_Expr_FAdd _ref1 _ref2 )
sem_Expr (FDiv _ref1 _ref2 )  =
    (sem_Expr_FDiv _ref1 _ref2 )
sem_Expr (FMul _ref1 _ref2 )  =
    (sem_Expr_FMul _ref1 _ref2 )
sem_Expr (FNeg _ref )  =
    (sem_Expr_FNeg _ref )
sem_Expr (FSub _ref1 _ref2 )  =
    (sem_Expr_FSub _ref1 _ref2 )
sem_Expr (Float _val )  =
    (sem_Expr_Float _val )
sem_Expr (Get _aref _iref )  =
    (sem_Expr_Get _aref _iref )
sem_Expr (IfEq _ref1 _ref2 _texpr _eexpr )  =
    (sem_Expr_IfEq _ref1 _ref2 (sem_Expr _texpr ) (sem_Expr _eexpr ) )
sem_Expr (IfLE _ref1 _ref2 _texpr _eexpr )  =
    (sem_Expr_IfLE _ref1 _ref2 (sem_Expr _texpr ) (sem_Expr _eexpr ) )
sem_Expr (Int _val )  =
    (sem_Expr_Int _val )
sem_Expr (Let _tyid _sub _body )  =
    (sem_Expr_Let (sem_TypeId _tyid ) (sem_Expr _sub ) (sem_Expr _body ) )
sem_Expr (LetRec _fundef _body )  =
    (sem_Expr_LetRec (sem_Fundef _fundef ) (sem_Expr _body ) )
sem_Expr (LetTuple _tyids _ref _body )  =
    (sem_Expr_LetTuple (sem_TypeIds _tyids ) _ref (sem_Expr _body ) )
sem_Expr (Neg _ref )  =
    (sem_Expr_Neg _ref )
sem_Expr (Put _aref _iref _vref )  =
    (sem_Expr_Put _aref _iref _vref )
sem_Expr (Sub _ref1 _ref2 )  =
    (sem_Expr_Sub _ref1 _ref2 )
sem_Expr (Tuple _refs )  =
    (sem_Expr_Tuple _refs )
sem_Expr (Unit )  =
    (sem_Expr_Unit )
sem_Expr (Var _ref )  =
    (sem_Expr_Var _ref )
-- semantic domain
type T_Expr  = Env ->
               Int ->
               ( Expr,Env,Int)
data Inh_Expr  = Inh_Expr {env_Inh_Expr :: Env,fresh_Inh_Expr :: Int}
data Syn_Expr  = Syn_Expr {alpha_Syn_Expr :: Expr,env_Syn_Expr :: Env,fresh_Syn_Expr :: Int}
wrap_Expr :: T_Expr  ->
             Inh_Expr  ->
             Syn_Expr 
wrap_Expr sem (Inh_Expr _lhsIenv _lhsIfresh )  =
    (let ( _lhsOalpha,_lhsOenv,_lhsOfresh) =
             (sem _lhsIenv _lhsIfresh )
     in  (Syn_Expr _lhsOalpha _lhsOenv _lhsOfresh ))
sem_Expr_Add :: Id ->
                Id ->
                T_Expr 
sem_Expr_Add ref1_ ref2_  =
    (\ _lhsIenv
       _lhsIfresh ->
         (let _lhsOalpha :: Expr
              _lhsOenv :: Env
              _lhsOfresh :: Int
              -- "Alpha.ag"(line 52, column 17)
              _lhsOalpha =
                  Add  (ref1_ &? _lhsIenv) (ref2_ &? _lhsIenv)
              -- copy rule (chain)
              _lhsOenv =
                  _lhsIenv
              -- copy rule (chain)
              _lhsOfresh =
                  _lhsIfresh
          in  ( _lhsOalpha,_lhsOenv,_lhsOfresh)))
sem_Expr_App :: Id ->
                ([Id]) ->
                T_Expr 
sem_Expr_App ref_ args_  =
    (\ _lhsIenv
       _lhsIfresh ->
         (let _lhsOalpha :: Expr
              _lhsOenv :: Env
              _lhsOfresh :: Int
              -- "Alpha.ag"(line 76, column 17)
              _lhsOalpha =
                  App (ref_ &? _lhsIenv)
                      (map (&? _lhsIenv) args_)
              -- copy rule (chain)
              _lhsOenv =
                  _lhsIenv
              -- copy rule (chain)
              _lhsOfresh =
                  _lhsIfresh
          in  ( _lhsOalpha,_lhsOenv,_lhsOfresh)))
sem_Expr_ExtArray :: Id ->
                     T_Expr 
sem_Expr_ExtArray ref_  =
    (\ _lhsIenv
       _lhsIfresh ->
         (let _lhsOalpha :: Expr
              _lhsOenv :: Env
              _lhsOfresh :: Int
              -- "Alpha.ag"(line 89, column 17)
              _lhsOalpha =
                  ExtArray ref_
              -- copy rule (chain)
              _lhsOenv =
                  _lhsIenv
              -- copy rule (chain)
              _lhsOfresh =
                  _lhsIfresh
          in  ( _lhsOalpha,_lhsOenv,_lhsOfresh)))
sem_Expr_ExtFunApp :: Id ->
                      ([Id]) ->
                      T_Expr 
sem_Expr_ExtFunApp nref_ args_  =
    (\ _lhsIenv
       _lhsIfresh ->
         (let _lhsOalpha :: Expr
              _lhsOenv :: Env
              _lhsOfresh :: Int
              -- "Alpha.ag"(line 90, column 17)
              _lhsOalpha =
                  ExtFunApp nref_ (map (&? _lhsIenv) args_)
              -- copy rule (chain)
              _lhsOenv =
                  _lhsIenv
              -- copy rule (chain)
              _lhsOfresh =
                  _lhsIfresh
          in  ( _lhsOalpha,_lhsOenv,_lhsOfresh)))
sem_Expr_FAdd :: Id ->
                 Id ->
                 T_Expr 
sem_Expr_FAdd ref1_ ref2_  =
    (\ _lhsIenv
       _lhsIfresh ->
         (let _lhsOalpha :: Expr
              _lhsOenv :: Env
              _lhsOfresh :: Int
              -- "Alpha.ag"(line 55, column 17)
              _lhsOalpha =
                  FAdd (ref1_ &? _lhsIenv) (ref2_ &? _lhsIenv)
              -- copy rule (chain)
              _lhsOenv =
                  _lhsIenv
              -- copy rule (chain)
              _lhsOfresh =
                  _lhsIfresh
          in  ( _lhsOalpha,_lhsOenv,_lhsOfresh)))
sem_Expr_FDiv :: Id ->
                 Id ->
                 T_Expr 
sem_Expr_FDiv ref1_ ref2_  =
    (\ _lhsIenv
       _lhsIfresh ->
         (let _lhsOalpha :: Expr
              _lhsOenv :: Env
              _lhsOfresh :: Int
              -- "Alpha.ag"(line 58, column 17)
              _lhsOalpha =
                  FDiv (ref1_ &? _lhsIenv) (ref2_ &? _lhsIenv)
              -- copy rule (chain)
              _lhsOenv =
                  _lhsIenv
              -- copy rule (chain)
              _lhsOfresh =
                  _lhsIfresh
          in  ( _lhsOalpha,_lhsOenv,_lhsOfresh)))
sem_Expr_FMul :: Id ->
                 Id ->
                 T_Expr 
sem_Expr_FMul ref1_ ref2_  =
    (\ _lhsIenv
       _lhsIfresh ->
         (let _lhsOalpha :: Expr
              _lhsOenv :: Env
              _lhsOfresh :: Int
              -- "Alpha.ag"(line 57, column 17)
              _lhsOalpha =
                  FMul (ref1_ &? _lhsIenv) (ref2_ &? _lhsIenv)
              -- copy rule (chain)
              _lhsOenv =
                  _lhsIenv
              -- copy rule (chain)
              _lhsOfresh =
                  _lhsIfresh
          in  ( _lhsOalpha,_lhsOenv,_lhsOfresh)))
sem_Expr_FNeg :: Id ->
                 T_Expr 
sem_Expr_FNeg ref_  =
    (\ _lhsIenv
       _lhsIfresh ->
         (let _lhsOalpha :: Expr
              _lhsOenv :: Env
              _lhsOfresh :: Int
              -- "Alpha.ag"(line 54, column 17)
              _lhsOalpha =
                  FNeg (ref_  &? _lhsIenv)
              -- copy rule (chain)
              _lhsOenv =
                  _lhsIenv
              -- copy rule (chain)
              _lhsOfresh =
                  _lhsIfresh
          in  ( _lhsOalpha,_lhsOenv,_lhsOfresh)))
sem_Expr_FSub :: Id ->
                 Id ->
                 T_Expr 
sem_Expr_FSub ref1_ ref2_  =
    (\ _lhsIenv
       _lhsIfresh ->
         (let _lhsOalpha :: Expr
              _lhsOenv :: Env
              _lhsOfresh :: Int
              -- "Alpha.ag"(line 56, column 17)
              _lhsOalpha =
                  FSub (ref1_ &? _lhsIenv) (ref2_ &? _lhsIenv)
              -- copy rule (chain)
              _lhsOenv =
                  _lhsIenv
              -- copy rule (chain)
              _lhsOfresh =
                  _lhsIfresh
          in  ( _lhsOalpha,_lhsOenv,_lhsOfresh)))
sem_Expr_Float :: Float ->
                  T_Expr 
sem_Expr_Float val_  =
    (\ _lhsIenv
       _lhsIfresh ->
         (let _lhsOalpha :: Expr
              _lhsOenv :: Env
              _lhsOfresh :: Int
              -- "Alpha.ag"(line 50, column 17)
              _lhsOalpha =
                  Float val_
              -- copy rule (chain)
              _lhsOenv =
                  _lhsIenv
              -- copy rule (chain)
              _lhsOfresh =
                  _lhsIfresh
          in  ( _lhsOalpha,_lhsOenv,_lhsOfresh)))
sem_Expr_Get :: Id ->
                Id ->
                T_Expr 
sem_Expr_Get aref_ iref_  =
    (\ _lhsIenv
       _lhsIfresh ->
         (let _lhsOalpha :: Expr
              _lhsOenv :: Env
              _lhsOfresh :: Int
              -- "Alpha.ag"(line 85, column 17)
              _lhsOalpha =
                  Get (aref_ &? _lhsIenv) (iref_ &? _lhsIenv)
              -- copy rule (chain)
              _lhsOenv =
                  _lhsIenv
              -- copy rule (chain)
              _lhsOfresh =
                  _lhsIfresh
          in  ( _lhsOalpha,_lhsOenv,_lhsOfresh)))
sem_Expr_IfEq :: Id ->
                 Id ->
                 T_Expr  ->
                 T_Expr  ->
                 T_Expr 
sem_Expr_IfEq ref1_ ref2_ texpr_ eexpr_  =
    (\ _lhsIenv
       _lhsIfresh ->
         (let _lhsOalpha :: Expr
              _lhsOenv :: Env
              _lhsOfresh :: Int
              _texprOenv :: Env
              _texprOfresh :: Int
              _eexprOenv :: Env
              _eexprOfresh :: Int
              _texprIalpha :: Expr
              _texprIenv :: Env
              _texprIfresh :: Int
              _eexprIalpha :: Expr
              _eexprIenv :: Env
              _eexprIfresh :: Int
              -- "Alpha.ag"(line 59, column 17)
              _lhsOalpha =
                  IfEq (ref1_ &? _lhsIenv)
                       (ref2_ &? _lhsIenv)
                       _texprIalpha
                       _eexprIalpha
              -- copy rule (up)
              _lhsOenv =
                  _eexprIenv
              -- copy rule (up)
              _lhsOfresh =
                  _eexprIfresh
              -- copy rule (down)
              _texprOenv =
                  _lhsIenv
              -- copy rule (down)
              _texprOfresh =
                  _lhsIfresh
              -- copy rule (chain)
              _eexprOenv =
                  _texprIenv
              -- copy rule (chain)
              _eexprOfresh =
                  _texprIfresh
              ( _texprIalpha,_texprIenv,_texprIfresh) =
                  (texpr_ _texprOenv _texprOfresh )
              ( _eexprIalpha,_eexprIenv,_eexprIfresh) =
                  (eexpr_ _eexprOenv _eexprOfresh )
          in  ( _lhsOalpha,_lhsOenv,_lhsOfresh)))
sem_Expr_IfLE :: Id ->
                 Id ->
                 T_Expr  ->
                 T_Expr  ->
                 T_Expr 
sem_Expr_IfLE ref1_ ref2_ texpr_ eexpr_  =
    (\ _lhsIenv
       _lhsIfresh ->
         (let _lhsOalpha :: Expr
              _lhsOenv :: Env
              _lhsOfresh :: Int
              _texprOenv :: Env
              _texprOfresh :: Int
              _eexprOenv :: Env
              _eexprOfresh :: Int
              _texprIalpha :: Expr
              _texprIenv :: Env
              _texprIfresh :: Int
              _eexprIalpha :: Expr
              _eexprIenv :: Env
              _eexprIfresh :: Int
              -- "Alpha.ag"(line 63, column 17)
              _lhsOalpha =
                  IfLE (ref1_ &? _lhsIenv)
                       (ref2_ &? _lhsIenv)
                       _texprIalpha
                       _eexprIalpha
              -- copy rule (up)
              _lhsOenv =
                  _eexprIenv
              -- copy rule (up)
              _lhsOfresh =
                  _eexprIfresh
              -- copy rule (down)
              _texprOenv =
                  _lhsIenv
              -- copy rule (down)
              _texprOfresh =
                  _lhsIfresh
              -- copy rule (chain)
              _eexprOenv =
                  _texprIenv
              -- copy rule (chain)
              _eexprOfresh =
                  _texprIfresh
              ( _texprIalpha,_texprIenv,_texprIfresh) =
                  (texpr_ _texprOenv _texprOfresh )
              ( _eexprIalpha,_eexprIenv,_eexprIfresh) =
                  (eexpr_ _eexprOenv _eexprOfresh )
          in  ( _lhsOalpha,_lhsOenv,_lhsOfresh)))
sem_Expr_Int :: Int ->
                T_Expr 
sem_Expr_Int val_  =
    (\ _lhsIenv
       _lhsIfresh ->
         (let _lhsOalpha :: Expr
              _lhsOenv :: Env
              _lhsOfresh :: Int
              -- "Alpha.ag"(line 49, column 17)
              _lhsOalpha =
                  Int val_
              -- copy rule (chain)
              _lhsOenv =
                  _lhsIenv
              -- copy rule (chain)
              _lhsOfresh =
                  _lhsIfresh
          in  ( _lhsOalpha,_lhsOenv,_lhsOfresh)))
sem_Expr_Let :: T_TypeId  ->
                T_Expr  ->
                T_Expr  ->
                T_Expr 
sem_Expr_Let tyid_ sub_ body_  =
    (\ _lhsIenv
       _lhsIfresh ->
         (let _bodyOenv :: Env
              _lhsOalpha :: Expr
              _lhsOenv :: Env
              _lhsOfresh :: Int
              _tyidOenv :: Env
              _tyidOfresh :: Int
              _subOenv :: Env
              _subOfresh :: Int
              _bodyOfresh :: Int
              _tyidIalpha :: TypeId
              _tyidIargId :: Id
              _tyidIcopy :: TypeId
              _tyidIenv :: Env
              _tyidIfresh :: Int
              _tyidIrelabel :: TypeId
              _tyidIrenames :: ((Id,Id))
              _subIalpha :: Expr
              _subIenv :: Env
              _subIfresh :: Int
              _bodyIalpha :: Expr
              _bodyIenv :: Env
              _bodyIfresh :: Int
              -- "Alpha.ag"(line 67, column 17)
              _newlab =
                  _tyidIrelabel
              -- "Alpha.ag"(line 68, column 17)
              _bodyOenv =
                  _tyidIrenames &+ _lhsIenv
              -- "Alpha.ag"(line 69, column 17)
              _lhsOalpha =
                  Let _newlab
                      _subIalpha
                      _bodyIalpha
              -- copy rule (up)
              _lhsOenv =
                  _bodyIenv
              -- copy rule (up)
              _lhsOfresh =
                  _bodyIfresh
              -- copy rule (down)
              _tyidOenv =
                  _lhsIenv
              -- copy rule (down)
              _tyidOfresh =
                  _lhsIfresh
              -- copy rule (chain)
              _subOenv =
                  _tyidIenv
              -- copy rule (chain)
              _subOfresh =
                  _tyidIfresh
              -- copy rule (chain)
              _bodyOfresh =
                  _subIfresh
              ( _tyidIalpha,_tyidIargId,_tyidIcopy,_tyidIenv,_tyidIfresh,_tyidIrelabel,_tyidIrenames) =
                  (tyid_ _tyidOenv _tyidOfresh )
              ( _subIalpha,_subIenv,_subIfresh) =
                  (sub_ _subOenv _subOfresh )
              ( _bodyIalpha,_bodyIenv,_bodyIfresh) =
                  (body_ _bodyOenv _bodyOfresh )
          in  ( _lhsOalpha,_lhsOenv,_lhsOfresh)))
sem_Expr_LetRec :: T_Fundef  ->
                   T_Expr  ->
                   T_Expr 
sem_Expr_LetRec fundef_ body_  =
    (\ _lhsIenv
       _lhsIfresh ->
         (let _lhsOenv :: Env
              _bodyOenv :: Env
              _lhsOalpha :: Expr
              _lhsOfresh :: Int
              _fundefOenv :: Env
              _fundefOfresh :: Int
              _bodyOfresh :: Int
              _fundefIalpha :: Fundef
              _fundefIenv :: Env
              _fundefIextenv :: Env
              _fundefIfresh :: Int
              _fundefIrenames :: ((Id,Id))
              _bodyIalpha :: Expr
              _bodyIenv :: Env
              _bodyIfresh :: Int
              -- "Alpha.ag"(line 73, column 17)
              _lhsOenv =
                  _fundefIextenv
              -- "Alpha.ag"(line 74, column 17)
              _bodyOenv =
                  _lhsIenv
              -- "Alpha.ag"(line 75, column 17)
              _lhsOalpha =
                  LetRec _fundefIalpha _bodyIalpha
              -- copy rule (up)
              _lhsOfresh =
                  _bodyIfresh
              -- copy rule (down)
              _fundefOenv =
                  _lhsIenv
              -- copy rule (down)
              _fundefOfresh =
                  _lhsIfresh
              -- copy rule (chain)
              _bodyOfresh =
                  _fundefIfresh
              ( _fundefIalpha,_fundefIenv,_fundefIextenv,_fundefIfresh,_fundefIrenames) =
                  (fundef_ _fundefOenv _fundefOfresh )
              ( _bodyIalpha,_bodyIenv,_bodyIfresh) =
                  (body_ _bodyOenv _bodyOfresh )
          in  ( _lhsOalpha,_lhsOenv,_lhsOfresh)))
sem_Expr_LetTuple :: T_TypeIds  ->
                     Id ->
                     T_Expr  ->
                     T_Expr 
sem_Expr_LetTuple tyids_ ref_ body_  =
    (\ _lhsIenv
       _lhsIfresh ->
         (let _tyidsOenv :: Env
              _bodyOenv :: Env
              _lhsOalpha :: Expr
              _lhsOenv :: Env
              _lhsOfresh :: Int
              _tyidsOfresh :: Int
              _bodyOfresh :: Int
              _tyidsIalpha :: ([TypeId])
              _tyidsIcopy :: TypeIds
              _tyidsIenv :: Env
              _tyidsIfresh :: Int
              _tyidsIrelabel :: ([TypeId])
              _tyidsIrenames :: ([(Id,Id)])
              _bodyIalpha :: Expr
              _bodyIenv :: Env
              _bodyIfresh :: Int
              -- "Alpha.ag"(line 79, column 17)
              _env' =
                  M.addList _tyidsIrenames _lhsIenv
              -- "Alpha.ag"(line 80, column 17)
              _tyidsOenv =
                  _env'
              -- "Alpha.ag"(line 81, column 17)
              _bodyOenv =
                  _env'
              -- "Alpha.ag"(line 82, column 17)
              _lhsOalpha =
                  LetTuple _tyidsIalpha
                           (ref_ &? _lhsIenv)
                           _bodyIalpha
              -- copy rule (up)
              _lhsOenv =
                  _bodyIenv
              -- copy rule (up)
              _lhsOfresh =
                  _bodyIfresh
              -- copy rule (down)
              _tyidsOfresh =
                  _lhsIfresh
              -- copy rule (chain)
              _bodyOfresh =
                  _tyidsIfresh
              ( _tyidsIalpha,_tyidsIcopy,_tyidsIenv,_tyidsIfresh,_tyidsIrelabel,_tyidsIrenames) =
                  (tyids_ _tyidsOenv _tyidsOfresh )
              ( _bodyIalpha,_bodyIenv,_bodyIfresh) =
                  (body_ _bodyOenv _bodyOfresh )
          in  ( _lhsOalpha,_lhsOenv,_lhsOfresh)))
sem_Expr_Neg :: Id ->
                T_Expr 
sem_Expr_Neg ref_  =
    (\ _lhsIenv
       _lhsIfresh ->
         (let _lhsOalpha :: Expr
              _lhsOenv :: Env
              _lhsOfresh :: Int
              -- "Alpha.ag"(line 51, column 17)
              _lhsOalpha =
                  Neg  (ref_  &? _lhsIenv)
              -- copy rule (chain)
              _lhsOenv =
                  _lhsIenv
              -- copy rule (chain)
              _lhsOfresh =
                  _lhsIfresh
          in  ( _lhsOalpha,_lhsOenv,_lhsOfresh)))
sem_Expr_Put :: Id ->
                Id ->
                Id ->
                T_Expr 
sem_Expr_Put aref_ iref_ vref_  =
    (\ _lhsIenv
       _lhsIfresh ->
         (let _lhsOalpha :: Expr
              _lhsOenv :: Env
              _lhsOfresh :: Int
              -- "Alpha.ag"(line 86, column 17)
              _lhsOalpha =
                  Put (aref_ &? _lhsIenv)
                      (iref_ &? _lhsIenv)
                      (vref_ &? _lhsIenv)
              -- copy rule (chain)
              _lhsOenv =
                  _lhsIenv
              -- copy rule (chain)
              _lhsOfresh =
                  _lhsIfresh
          in  ( _lhsOalpha,_lhsOenv,_lhsOfresh)))
sem_Expr_Sub :: Id ->
                Id ->
                T_Expr 
sem_Expr_Sub ref1_ ref2_  =
    (\ _lhsIenv
       _lhsIfresh ->
         (let _lhsOalpha :: Expr
              _lhsOenv :: Env
              _lhsOfresh :: Int
              -- "Alpha.ag"(line 53, column 17)
              _lhsOalpha =
                  Sub  (ref1_ &? _lhsIenv) (ref2_ &? _lhsIenv)
              -- copy rule (chain)
              _lhsOenv =
                  _lhsIenv
              -- copy rule (chain)
              _lhsOfresh =
                  _lhsIfresh
          in  ( _lhsOalpha,_lhsOenv,_lhsOfresh)))
sem_Expr_Tuple :: ([Id]) ->
                  T_Expr 
sem_Expr_Tuple refs_  =
    (\ _lhsIenv
       _lhsIfresh ->
         (let _lhsOalpha :: Expr
              _lhsOenv :: Env
              _lhsOfresh :: Int
              -- "Alpha.ag"(line 78, column 17)
              _lhsOalpha =
                  Tuple (map (&? _lhsIenv) refs_)
              -- copy rule (chain)
              _lhsOenv =
                  _lhsIenv
              -- copy rule (chain)
              _lhsOfresh =
                  _lhsIfresh
          in  ( _lhsOalpha,_lhsOenv,_lhsOfresh)))
sem_Expr_Unit :: T_Expr 
sem_Expr_Unit  =
    (\ _lhsIenv
       _lhsIfresh ->
         (let _lhsOalpha :: Expr
              _lhsOenv :: Env
              _lhsOfresh :: Int
              -- "Alpha.ag"(line 48, column 17)
              _lhsOalpha =
                  Unit
              -- copy rule (chain)
              _lhsOenv =
                  _lhsIenv
              -- copy rule (chain)
              _lhsOfresh =
                  _lhsIfresh
          in  ( _lhsOalpha,_lhsOenv,_lhsOfresh)))
sem_Expr_Var :: Id ->
                T_Expr 
sem_Expr_Var ref_  =
    (\ _lhsIenv
       _lhsIfresh ->
         (let _lhsOalpha :: Expr
              _lhsOenv :: Env
              _lhsOfresh :: Int
              -- "Alpha.ag"(line 72, column 17)
              _lhsOalpha =
                  Var (ref_ &? _lhsIenv)
              -- copy rule (chain)
              _lhsOenv =
                  _lhsIenv
              -- copy rule (chain)
              _lhsOfresh =
                  _lhsIfresh
          in  ( _lhsOalpha,_lhsOenv,_lhsOfresh)))
-- Fundef ------------------------------------------------------
{-
   visit 0:
      chained attributes:
         env                  : Env
         fresh                : Int
      synthesized attributes:
         alpha                : Fundef
         extenv               : Env
         renames              : (Id,Id)
   alternatives:
      alternative Fundef:
         child name           : TypeId 
         child args           : TypeIds 
         child body           : Expr 
         visit 0:
            local env'        : _
            local extenvs     : _
-}
-- cata
sem_Fundef :: Fundef  ->
              T_Fundef 
sem_Fundef (Fundef _name _args _body )  =
    (sem_Fundef_Fundef (sem_TypeId _name ) (sem_TypeIds _args ) (sem_Expr _body ) )
-- semantic domain
type T_Fundef  = Env ->
                 Int ->
                 ( Fundef,Env,Env,Int,((Id,Id)))
data Inh_Fundef  = Inh_Fundef {env_Inh_Fundef :: Env,fresh_Inh_Fundef :: Int}
data Syn_Fundef  = Syn_Fundef {alpha_Syn_Fundef :: Fundef,env_Syn_Fundef :: Env,extenv_Syn_Fundef :: Env,fresh_Syn_Fundef :: Int,renames_Syn_Fundef :: (Id,Id)}
wrap_Fundef :: T_Fundef  ->
               Inh_Fundef  ->
               Syn_Fundef 
wrap_Fundef sem (Inh_Fundef _lhsIenv _lhsIfresh )  =
    (let ( _lhsOalpha,_lhsOenv,_lhsOextenv,_lhsOfresh,_lhsOrenames) =
             (sem _lhsIenv _lhsIfresh )
     in  (Syn_Fundef _lhsOalpha _lhsOenv _lhsOextenv _lhsOfresh _lhsOrenames ))
sem_Fundef_Fundef :: T_TypeId  ->
                     T_TypeIds  ->
                     T_Expr  ->
                     T_Fundef 
sem_Fundef_Fundef name_ args_ body_  =
    (\ _lhsIenv
       _lhsIfresh ->
         (let _lhsOrenames :: ((Id,Id))
              _lhsOextenv :: Env
              _bodyOenv :: Env
              _lhsOalpha :: Fundef
              _lhsOenv :: Env
              _lhsOfresh :: Int
              _nameOenv :: Env
              _nameOfresh :: Int
              _argsOenv :: Env
              _argsOfresh :: Int
              _bodyOfresh :: Int
              _nameIalpha :: TypeId
              _nameIargId :: Id
              _nameIcopy :: TypeId
              _nameIenv :: Env
              _nameIfresh :: Int
              _nameIrelabel :: TypeId
              _nameIrenames :: ((Id,Id))
              _argsIalpha :: ([TypeId])
              _argsIcopy :: TypeIds
              _argsIenv :: Env
              _argsIfresh :: Int
              _argsIrelabel :: ([TypeId])
              _argsIrenames :: ([(Id,Id)])
              _bodyIalpha :: Expr
              _bodyIenv :: Env
              _bodyIfresh :: Int
              -- "Alpha.ag"(line 113, column 17)
              _lhsOrenames =
                  _nameIrenames
              -- "Alpha.ag"(line 118, column 17)
              _env' =
                  _nameIrenames &+ _lhsIenv
              -- "Alpha.ag"(line 119, column 17)
              _lhsOextenv =
                  _env'
              -- "Alpha.ag"(line 120, column 17)
              _extenvs =
                  M.addList _argsIrenames _env'
              -- "Alpha.ag"(line 121, column 17)
              _bodyOenv =
                  _extenvs
              -- "Alpha.ag"(line 122, column 17)
              _lhsOalpha =
                  Fundef _nameIrelabel
                         _argsIalpha
                         _bodyIalpha
              -- copy rule (up)
              _lhsOenv =
                  _bodyIenv
              -- copy rule (up)
              _lhsOfresh =
                  _bodyIfresh
              -- copy rule (down)
              _nameOenv =
                  _lhsIenv
              -- copy rule (down)
              _nameOfresh =
                  _lhsIfresh
              -- copy rule (chain)
              _argsOenv =
                  _nameIenv
              -- copy rule (chain)
              _argsOfresh =
                  _nameIfresh
              -- copy rule (chain)
              _bodyOfresh =
                  _argsIfresh
              ( _nameIalpha,_nameIargId,_nameIcopy,_nameIenv,_nameIfresh,_nameIrelabel,_nameIrenames) =
                  (name_ _nameOenv _nameOfresh )
              ( _argsIalpha,_argsIcopy,_argsIenv,_argsIfresh,_argsIrelabel,_argsIrenames) =
                  (args_ _argsOenv _argsOfresh )
              ( _bodyIalpha,_bodyIenv,_bodyIfresh) =
                  (body_ _bodyOenv _bodyOfresh )
          in  ( _lhsOalpha,_lhsOenv,_lhsOextenv,_lhsOfresh,_lhsOrenames)))
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
      chained attributes:
         env                  : Env
         fresh                : Int
      synthesized attributes:
         alpha                : TypeId
         argId                : Id
         copy                 : SELF 
         relabel              : TypeId
         renames              : (Id,Id)
   alternatives:
      alternative Tuple:
         child argId          : {Id}
         child argType        : Type 
         visit 0:
            local _tup1       : _
            local x           : _
            local x'          : _
            local copy        : _
-}
-- cata
sem_TypeId :: TypeId  ->
              T_TypeId 
sem_TypeId ( argId,argType)  =
    (sem_TypeId_Tuple argId (sem_Type argType ) )
-- semantic domain
type T_TypeId  = Env ->
                 Int ->
                 ( TypeId,Id,TypeId,Env,Int,TypeId,((Id,Id)))
data Inh_TypeId  = Inh_TypeId {env_Inh_TypeId :: Env,fresh_Inh_TypeId :: Int}
data Syn_TypeId  = Syn_TypeId {alpha_Syn_TypeId :: TypeId,argId_Syn_TypeId :: Id,copy_Syn_TypeId :: TypeId,env_Syn_TypeId :: Env,fresh_Syn_TypeId :: Int,relabel_Syn_TypeId :: TypeId,renames_Syn_TypeId :: (Id,Id)}
wrap_TypeId :: T_TypeId  ->
               Inh_TypeId  ->
               Syn_TypeId 
wrap_TypeId sem (Inh_TypeId _lhsIenv _lhsIfresh )  =
    (let ( _lhsOalpha,_lhsOargId,_lhsOcopy,_lhsOenv,_lhsOfresh,_lhsOrelabel,_lhsOrenames) =
             (sem _lhsIenv _lhsIfresh )
     in  (Syn_TypeId _lhsOalpha _lhsOargId _lhsOcopy _lhsOenv _lhsOfresh _lhsOrelabel _lhsOrenames ))
sem_TypeId_Tuple :: Id ->
                    T_Type  ->
                    T_TypeId 
sem_TypeId_Tuple argId_ argType_  =
    (\ _lhsIenv
       _lhsIfresh ->
         (let _lhsOalpha :: TypeId
              _lhsOfresh :: Int
              _lhsOrenames :: ((Id,Id))
              _lhsOrelabel :: TypeId
              _lhsOargId :: Id
              _lhsOcopy :: TypeId
              _lhsOenv :: Env
              _argTypeIcopy :: Type
              -- "Alpha.ag"(line 93, column 17)
              _lhsOalpha =
                  (argId_ &? _lhsIenv, _argTypeIcopy)
              -- "Alpha.ag"(line 104, column 17)
              _lhsOfresh =
                  _lhsIfresh+1
              -- "Alpha.ag"(line 105, column 21)
              __tup1 =
                  (argId_, newId argId_ _lhsIfresh)
              -- "Alpha.ag"(line 105, column 21)
              (_x,_) =
                  __tup1
              -- "Alpha.ag"(line 105, column 21)
              (_,_x') =
                  __tup1
              -- "Alpha.ag"(line 106, column 17)
              _lhsOrenames =
                  (_x    ,_x'    )
              -- "Alpha.ag"(line 109, column 17)
              _lhsOrelabel =
                  (_x'    , _argTypeIcopy)
              -- "./TypeDEFS.ag"(line 29, column 15)
              _lhsOargId =
                  argId_
              -- self rule
              _copy =
                  (argId_,_argTypeIcopy)
              -- self rule
              _lhsOcopy =
                  _copy
              -- copy rule (chain)
              _lhsOenv =
                  _lhsIenv
              ( _argTypeIcopy) =
                  (argType_ )
          in  ( _lhsOalpha,_lhsOargId,_lhsOcopy,_lhsOenv,_lhsOfresh,_lhsOrelabel,_lhsOrenames)))
-- TypeIds -----------------------------------------------------
{-
   visit 0:
      chained attributes:
         env                  : Env
         fresh                : Int
      synthesized attributes:
         alpha                : [TypeId]
         copy                 : SELF 
         relabel              : [TypeId]
         renames              : [(Id,Id)]
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
type T_TypeIds  = Env ->
                  Int ->
                  ( ([TypeId]),TypeIds,Env,Int,([TypeId]),([(Id,Id)]))
data Inh_TypeIds  = Inh_TypeIds {env_Inh_TypeIds :: Env,fresh_Inh_TypeIds :: Int}
data Syn_TypeIds  = Syn_TypeIds {alpha_Syn_TypeIds :: [TypeId],copy_Syn_TypeIds :: TypeIds,env_Syn_TypeIds :: Env,fresh_Syn_TypeIds :: Int,relabel_Syn_TypeIds :: [TypeId],renames_Syn_TypeIds :: [(Id,Id)]}
wrap_TypeIds :: T_TypeIds  ->
                Inh_TypeIds  ->
                Syn_TypeIds 
wrap_TypeIds sem (Inh_TypeIds _lhsIenv _lhsIfresh )  =
    (let ( _lhsOalpha,_lhsOcopy,_lhsOenv,_lhsOfresh,_lhsOrelabel,_lhsOrenames) =
             (sem _lhsIenv _lhsIfresh )
     in  (Syn_TypeIds _lhsOalpha _lhsOcopy _lhsOenv _lhsOfresh _lhsOrelabel _lhsOrenames ))
sem_TypeIds_Cons :: T_TypeId  ->
                    T_TypeIds  ->
                    T_TypeIds 
sem_TypeIds_Cons hd_ tl_  =
    (\ _lhsIenv
       _lhsIfresh ->
         (let _lhsOalpha :: ([TypeId])
              _lhsOrelabel :: ([TypeId])
              _lhsOrenames :: ([(Id,Id)])
              _lhsOcopy :: TypeIds
              _lhsOenv :: Env
              _lhsOfresh :: Int
              _hdOenv :: Env
              _hdOfresh :: Int
              _tlOenv :: Env
              _tlOfresh :: Int
              _hdIalpha :: TypeId
              _hdIargId :: Id
              _hdIcopy :: TypeId
              _hdIenv :: Env
              _hdIfresh :: Int
              _hdIrelabel :: TypeId
              _hdIrenames :: ((Id,Id))
              _tlIalpha :: ([TypeId])
              _tlIcopy :: TypeIds
              _tlIenv :: Env
              _tlIfresh :: Int
              _tlIrelabel :: ([TypeId])
              _tlIrenames :: ([(Id,Id)])
              -- use rule "Alpha.ag"(line 39, column 27)
              _lhsOalpha =
                  _hdIalpha : _tlIalpha
              -- use rule "Alpha.ag"(line 100, column 34)
              _lhsOrelabel =
                  _hdIrelabel : _tlIrelabel
              -- use rule "Alpha.ag"(line 97, column 34)
              _lhsOrenames =
                  _hdIrenames : _tlIrenames
              -- self rule
              _copy =
                  (:) _hdIcopy _tlIcopy
              -- self rule
              _lhsOcopy =
                  _copy
              -- copy rule (up)
              _lhsOenv =
                  _tlIenv
              -- copy rule (up)
              _lhsOfresh =
                  _tlIfresh
              -- copy rule (down)
              _hdOenv =
                  _lhsIenv
              -- copy rule (down)
              _hdOfresh =
                  _lhsIfresh
              -- copy rule (chain)
              _tlOenv =
                  _hdIenv
              -- copy rule (chain)
              _tlOfresh =
                  _hdIfresh
              ( _hdIalpha,_hdIargId,_hdIcopy,_hdIenv,_hdIfresh,_hdIrelabel,_hdIrenames) =
                  (hd_ _hdOenv _hdOfresh )
              ( _tlIalpha,_tlIcopy,_tlIenv,_tlIfresh,_tlIrelabel,_tlIrenames) =
                  (tl_ _tlOenv _tlOfresh )
          in  ( _lhsOalpha,_lhsOcopy,_lhsOenv,_lhsOfresh,_lhsOrelabel,_lhsOrenames)))
sem_TypeIds_Nil :: T_TypeIds 
sem_TypeIds_Nil  =
    (\ _lhsIenv
       _lhsIfresh ->
         (let _lhsOalpha :: ([TypeId])
              _lhsOrelabel :: ([TypeId])
              _lhsOrenames :: ([(Id,Id)])
              _lhsOcopy :: TypeIds
              _lhsOenv :: Env
              _lhsOfresh :: Int
              -- use rule "Alpha.ag"(line 39, column 27)
              _lhsOalpha =
                  []
              -- use rule "Alpha.ag"(line 100, column 34)
              _lhsOrelabel =
                  []
              -- use rule "Alpha.ag"(line 97, column 34)
              _lhsOrenames =
                  []
              -- self rule
              _copy =
                  []
              -- self rule
              _lhsOcopy =
                  _copy
              -- copy rule (chain)
              _lhsOenv =
                  _lhsIenv
              -- copy rule (chain)
              _lhsOfresh =
                  _lhsIfresh
          in  ( _lhsOalpha,_lhsOcopy,_lhsOenv,_lhsOfresh,_lhsOrelabel,_lhsOrenames)))
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