

-- UUAGC 0.9.6 (ConstFold.ag)


-- |
-- Module: HMinCaml.ConstFold
-- License: as per original MinCaml
--
-- Maintainer: Stephen Tetley <stephen.tetley@gmail.com>
-- Stability: unstable
-- Portability: ghc
--
-- Constant folding
--


module HMinCaml.ConstFold where


import HMinCaml.Id
import HMinCaml.KNormalSyn
import qualified HMinCaml.M as M
import HMinCaml.Type
import HMinCaml.Utils

type Env = M.M Id Expr

findInt :: Id -> Env -> Maybe Int
findInt s env = case M.find s env of
    Just (Int i) -> Just i
    _            -> Nothing

findFloat :: Id -> Env -> Maybe Float
findFloat s env = case M.find s env of
    Just (Float d) -> Just d
    _              -> Nothing

findTuple :: Id -> Env -> Maybe [Id]                 
findTuple x env = case M.find x env of
    Just (Tuple ys) -> Just ys
    _               -> Nothing
    


both :: Maybe a -> Maybe b -> Maybe (a,b)
both (Just a) (Just b) = Just (a,b) 
both _        _        = Nothing
      
alt :: Maybe a -> Maybe b -> Maybe (Either a b)
alt (Just a) _        = Just (Left a)
alt _        (Just b) = Just (Right b)
alt _        _        = Nothing





-- either-pair-equal
epeq :: (Eq a, Eq b) => Either (a,a) (b,b) -> Bool
epeq (Left  (a,b)) = a==b
epeq (Right (a,b)) = a==b

-- either-pair-less-than-or-equal
epLeq :: (Ord a, Ord b) => Either (a,a) (b,b) -> Bool
epLeq (Left  (a,b)) = a<=b
epLeq (Right (a,b)) = a<=b


                
constFold :: Expr -> Expr
constFold expr = constFold_Syn_Expr synthesized
  where
    synthesized = wrap_Expr (sem_Expr expr) inherited
    inherited = Inh_Expr { env_Inh_Expr = M.empty }

-- Expr --------------------------------------------------------
{-
   visit 0:
      chained attribute:
         env                  : Env
      synthesized attributes:
         constFold            : Expr
         copy                 : SELF 
   alternatives:
      alternative Add:
         child x              : {Id}
         child y              : {Id}
         visit 0:
            local copy        : _
      alternative App:
         child ref            : {Id}
         child args           : {[Id]}
         visit 0:
            local copy        : _
      alternative ExtArray:
         child ref            : {Id}
         visit 0:
            local copy        : _
      alternative ExtFunApp:
         child nref           : {Id}
         child args           : {[Id]}
         visit 0:
            local copy        : _
      alternative FAdd:
         child x              : {Id}
         child y              : {Id}
         visit 0:
            local copy        : _
      alternative FDiv:
         child x              : {Id}
         child y              : {Id}
         visit 0:
            local copy        : _
      alternative FMul:
         child x              : {Id}
         child y              : {Id}
         visit 0:
            local copy        : _
      alternative FNeg:
         child x              : {Id}
         visit 0:
            local copy        : _
      alternative FSub:
         child x              : {Id}
         child y              : {Id}
         visit 0:
            local copy        : _
      alternative Float:
         child val            : {Float}
         visit 0:
            local copy        : _
      alternative Get:
         child aref           : {Id}
         child iref           : {Id}
         visit 0:
            local copy        : _
      alternative IfEq:
         child x              : {Id}
         child y              : {Id}
         child texpr          : Expr 
         child eexpr          : Expr 
         visit 0:
            local texpr'      : _
            local eexpr'      : _
            local copy        : _
      alternative IfLE:
         child x              : {Id}
         child y              : {Id}
         child texpr          : Expr 
         child eexpr          : Expr 
         visit 0:
            local texpr'      : _
            local eexpr'      : _
            local copy        : _
      alternative Int:
         child val            : {Int}
         visit 0:
            local copy        : _
      alternative Let:
         child tyid           : TypeId 
         child sub            : Expr 
         child body           : Expr 
         visit 0:
            local sub'        : _
            local body'       : _
            local copy        : _
      alternative LetRec:
         child fundef         : Fundef 
         child body           : Expr 
         visit 0:
            local copy        : _
      alternative LetTuple:
         child tyids          : TypeIds 
         child ref            : {Id}
         child body           : Expr 
         visit 0:
            local copy        : _
      alternative Neg:
         child x              : {Id}
         visit 0:
            local copy        : _
      alternative Put:
         child aref           : {Id}
         child iref           : {Id}
         child vref           : {Id}
         visit 0:
            local copy        : _
      alternative Sub:
         child x              : {Id}
         child y              : {Id}
         visit 0:
            local copy        : _
      alternative Tuple:
         child refs           : {[Id]}
         visit 0:
            local copy        : _
      alternative Unit:
         visit 0:
            local copy        : _
      alternative Var:
         child ref            : {Id}
         visit 0:
            local copy        : _
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
    (sem_Expr_Let (sem_TypeId _tyid ) (sem_Expr _sub ) (sem_Expr _body ) )
sem_Expr (LetRec _fundef _body )  =
    (sem_Expr_LetRec (sem_Fundef _fundef ) (sem_Expr _body ) )
sem_Expr (LetTuple _tyids _ref _body )  =
    (sem_Expr_LetTuple (sem_TypeIds _tyids ) _ref (sem_Expr _body ) )
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
type T_Expr  = Env ->
               ( Expr,Expr,Env)
data Inh_Expr  = Inh_Expr {env_Inh_Expr :: Env}
data Syn_Expr  = Syn_Expr {constFold_Syn_Expr :: Expr,copy_Syn_Expr :: Expr,env_Syn_Expr :: Env}
wrap_Expr :: T_Expr  ->
             Inh_Expr  ->
             Syn_Expr 
wrap_Expr sem (Inh_Expr _lhsIenv )  =
    (let ( _lhsOconstFold,_lhsOcopy,_lhsOenv) =
             (sem _lhsIenv )
     in  (Syn_Expr _lhsOconstFold _lhsOcopy _lhsOenv ))
sem_Expr_Add :: Id ->
                Id ->
                T_Expr 
sem_Expr_Add x_ y_  =
    (\ _lhsIenv ->
         (let _lhsOconstFold :: Expr
              _lhsOcopy :: Expr
              _lhsOenv :: Env
              -- "ConstFold.ag"(line 71, column 11)
              _lhsOconstFold =
                  maybe _copy
                        (Int . uncurry (+))
                        (both (findInt x_ _lhsIenv)
                              (findInt y_ _lhsIenv))
              -- self rule
              _copy =
                  Add x_ y_
              -- self rule
              _lhsOcopy =
                  _copy
              -- copy rule (chain)
              _lhsOenv =
                  _lhsIenv
          in  ( _lhsOconstFold,_lhsOcopy,_lhsOenv)))
sem_Expr_App :: Id ->
                ([Id]) ->
                T_Expr 
sem_Expr_App ref_ args_  =
    (\ _lhsIenv ->
         (let _lhsOconstFold :: Expr
              _lhsOcopy :: Expr
              _lhsOenv :: Env
              -- "ConstFold.ag"(line 131, column 17)
              _lhsOconstFold =
                  _copy
              -- self rule
              _copy =
                  App ref_ args_
              -- self rule
              _lhsOcopy =
                  _copy
              -- copy rule (chain)
              _lhsOenv =
                  _lhsIenv
          in  ( _lhsOconstFold,_lhsOcopy,_lhsOenv)))
sem_Expr_ExtArray :: Id ->
                     T_Expr 
sem_Expr_ExtArray ref_  =
    (\ _lhsIenv ->
         (let _lhsOconstFold :: Expr
              _lhsOcopy :: Expr
              _lhsOenv :: Env
              -- "ConstFold.ag"(line 131, column 17)
              _lhsOconstFold =
                  _copy
              -- self rule
              _copy =
                  ExtArray ref_
              -- self rule
              _lhsOcopy =
                  _copy
              -- copy rule (chain)
              _lhsOenv =
                  _lhsIenv
          in  ( _lhsOconstFold,_lhsOcopy,_lhsOenv)))
sem_Expr_ExtFunApp :: Id ->
                      ([Id]) ->
                      T_Expr 
sem_Expr_ExtFunApp nref_ args_  =
    (\ _lhsIenv ->
         (let _lhsOconstFold :: Expr
              _lhsOcopy :: Expr
              _lhsOenv :: Env
              -- "ConstFold.ag"(line 131, column 17)
              _lhsOconstFold =
                  _copy
              -- self rule
              _copy =
                  ExtFunApp nref_ args_
              -- self rule
              _lhsOcopy =
                  _copy
              -- copy rule (chain)
              _lhsOenv =
                  _lhsIenv
          in  ( _lhsOconstFold,_lhsOcopy,_lhsOenv)))
sem_Expr_FAdd :: Id ->
                 Id ->
                 T_Expr 
sem_Expr_FAdd x_ y_  =
    (\ _lhsIenv ->
         (let _lhsOconstFold :: Expr
              _lhsOcopy :: Expr
              _lhsOenv :: Env
              -- "ConstFold.ag"(line 80, column 11)
              _lhsOconstFold =
                  maybe _copy
                        (Float . uncurry (+))
                        (both (findFloat x_ _lhsIenv)
                              (findFloat y_ _lhsIenv))
              -- self rule
              _copy =
                  FAdd x_ y_
              -- self rule
              _lhsOcopy =
                  _copy
              -- copy rule (chain)
              _lhsOenv =
                  _lhsIenv
          in  ( _lhsOconstFold,_lhsOcopy,_lhsOenv)))
sem_Expr_FDiv :: Id ->
                 Id ->
                 T_Expr 
sem_Expr_FDiv x_ y_  =
    (\ _lhsIenv ->
         (let _lhsOconstFold :: Expr
              _lhsOcopy :: Expr
              _lhsOenv :: Env
              -- "ConstFold.ag"(line 92, column 11)
              _lhsOconstFold =
                  maybe _copy
                        (Float . uncurry (/))
                        (both (findFloat x_ _lhsIenv)
                              (findFloat y_ _lhsIenv))
              -- self rule
              _copy =
                  FDiv x_ y_
              -- self rule
              _lhsOcopy =
                  _copy
              -- copy rule (chain)
              _lhsOenv =
                  _lhsIenv
          in  ( _lhsOconstFold,_lhsOcopy,_lhsOenv)))
sem_Expr_FMul :: Id ->
                 Id ->
                 T_Expr 
sem_Expr_FMul x_ y_  =
    (\ _lhsIenv ->
         (let _lhsOconstFold :: Expr
              _lhsOcopy :: Expr
              _lhsOenv :: Env
              -- "ConstFold.ag"(line 88, column 11)
              _lhsOconstFold =
                  maybe _copy
                        (Float . uncurry (*))
                        (both (findFloat x_ _lhsIenv)
                              (findFloat y_ _lhsIenv))
              -- self rule
              _copy =
                  FMul x_ y_
              -- self rule
              _lhsOcopy =
                  _copy
              -- copy rule (chain)
              _lhsOenv =
                  _lhsIenv
          in  ( _lhsOconstFold,_lhsOcopy,_lhsOenv)))
sem_Expr_FNeg :: Id ->
                 T_Expr 
sem_Expr_FNeg x_  =
    (\ _lhsIenv ->
         (let _lhsOconstFold :: Expr
              _lhsOcopy :: Expr
              _lhsOenv :: Env
              -- "ConstFold.ag"(line 79, column 11)
              _lhsOconstFold =
                  maybe _copy (Float . negate) (findFloat x_ _lhsIenv)
              -- self rule
              _copy =
                  FNeg x_
              -- self rule
              _lhsOcopy =
                  _copy
              -- copy rule (chain)
              _lhsOenv =
                  _lhsIenv
          in  ( _lhsOconstFold,_lhsOcopy,_lhsOenv)))
sem_Expr_FSub :: Id ->
                 Id ->
                 T_Expr 
sem_Expr_FSub x_ y_  =
    (\ _lhsIenv ->
         (let _lhsOconstFold :: Expr
              _lhsOcopy :: Expr
              _lhsOenv :: Env
              -- "ConstFold.ag"(line 84, column 11)
              _lhsOconstFold =
                  maybe _copy
                        (Float . uncurry (-))
                        (both (findFloat x_ _lhsIenv)
                              (findFloat y_ _lhsIenv))
              -- self rule
              _copy =
                  FSub x_ y_
              -- self rule
              _lhsOcopy =
                  _copy
              -- copy rule (chain)
              _lhsOenv =
                  _lhsIenv
          in  ( _lhsOconstFold,_lhsOcopy,_lhsOenv)))
sem_Expr_Float :: Float ->
                  T_Expr 
sem_Expr_Float val_  =
    (\ _lhsIenv ->
         (let _lhsOconstFold :: Expr
              _lhsOcopy :: Expr
              _lhsOenv :: Env
              -- "ConstFold.ag"(line 131, column 17)
              _lhsOconstFold =
                  _copy
              -- self rule
              _copy =
                  Float val_
              -- self rule
              _lhsOcopy =
                  _copy
              -- copy rule (chain)
              _lhsOenv =
                  _lhsIenv
          in  ( _lhsOconstFold,_lhsOcopy,_lhsOenv)))
sem_Expr_Get :: Id ->
                Id ->
                T_Expr 
sem_Expr_Get aref_ iref_  =
    (\ _lhsIenv ->
         (let _lhsOconstFold :: Expr
              _lhsOcopy :: Expr
              _lhsOenv :: Env
              -- "ConstFold.ag"(line 131, column 17)
              _lhsOconstFold =
                  _copy
              -- self rule
              _copy =
                  Get aref_ iref_
              -- self rule
              _lhsOcopy =
                  _copy
              -- copy rule (chain)
              _lhsOenv =
                  _lhsIenv
          in  ( _lhsOconstFold,_lhsOcopy,_lhsOenv)))
sem_Expr_IfEq :: Id ->
                 Id ->
                 T_Expr  ->
                 T_Expr  ->
                 T_Expr 
sem_Expr_IfEq x_ y_ texpr_ eexpr_  =
    (\ _lhsIenv ->
         (let _lhsOconstFold :: Expr
              _lhsOcopy :: Expr
              _lhsOenv :: Env
              _texprOenv :: Env
              _eexprOenv :: Env
              _texprIconstFold :: Expr
              _texprIcopy :: Expr
              _texprIenv :: Env
              _eexprIconstFold :: Expr
              _eexprIcopy :: Expr
              _eexprIenv :: Env
              -- "ConstFold.ag"(line 97, column 11)
              _texpr' =
                  _texprIconstFold
              -- "ConstFold.ag"(line 98, column 11)
              _eexpr' =
                  _eexprIconstFold
              -- "ConstFold.ag"(line 99, column 11)
              _lhsOconstFold =
                  maybe (IfEq x_ y_ _texpr'     _eexpr'    )
                        (\a -> if epeq a then _texpr'
                                         else _eexpr'    )
                        (alt (both (findInt   x_ _lhsIenv)
                                   (findInt   y_ _lhsIenv))
                             (both (findFloat x_ _lhsIenv)
                                   (findFloat y_ _lhsIenv)))
              -- self rule
              _copy =
                  IfEq x_ y_ _texprIcopy _eexprIcopy
              -- self rule
              _lhsOcopy =
                  _copy
              -- copy rule (up)
              _lhsOenv =
                  _eexprIenv
              -- copy rule (down)
              _texprOenv =
                  _lhsIenv
              -- copy rule (chain)
              _eexprOenv =
                  _texprIenv
              ( _texprIconstFold,_texprIcopy,_texprIenv) =
                  (texpr_ _texprOenv )
              ( _eexprIconstFold,_eexprIcopy,_eexprIenv) =
                  (eexpr_ _eexprOenv )
          in  ( _lhsOconstFold,_lhsOcopy,_lhsOenv)))
sem_Expr_IfLE :: Id ->
                 Id ->
                 T_Expr  ->
                 T_Expr  ->
                 T_Expr 
sem_Expr_IfLE x_ y_ texpr_ eexpr_  =
    (\ _lhsIenv ->
         (let _lhsOconstFold :: Expr
              _lhsOcopy :: Expr
              _lhsOenv :: Env
              _texprOenv :: Env
              _eexprOenv :: Env
              _texprIconstFold :: Expr
              _texprIcopy :: Expr
              _texprIenv :: Env
              _eexprIconstFold :: Expr
              _eexprIcopy :: Expr
              _eexprIenv :: Env
              -- "ConstFold.ag"(line 107, column 11)
              _texpr' =
                  _texprIconstFold
              -- "ConstFold.ag"(line 108, column 11)
              _eexpr' =
                  _eexprIconstFold
              -- "ConstFold.ag"(line 109, column 11)
              _lhsOconstFold =
                  maybe (IfLE x_ y_ _texpr'     _eexpr'    )
                        (\a -> if epLeq a then _texpr'
                                          else _eexpr'    )
                        (alt (both (findInt   x_ _lhsIenv)
                                   (findInt   y_ _lhsIenv))
                             (both (findFloat x_ _lhsIenv)
                                   (findFloat y_ _lhsIenv)))
              -- self rule
              _copy =
                  IfLE x_ y_ _texprIcopy _eexprIcopy
              -- self rule
              _lhsOcopy =
                  _copy
              -- copy rule (up)
              _lhsOenv =
                  _eexprIenv
              -- copy rule (down)
              _texprOenv =
                  _lhsIenv
              -- copy rule (chain)
              _eexprOenv =
                  _texprIenv
              ( _texprIconstFold,_texprIcopy,_texprIenv) =
                  (texpr_ _texprOenv )
              ( _eexprIconstFold,_eexprIcopy,_eexprIenv) =
                  (eexpr_ _eexprOenv )
          in  ( _lhsOconstFold,_lhsOcopy,_lhsOenv)))
sem_Expr_Int :: Int ->
                T_Expr 
sem_Expr_Int val_  =
    (\ _lhsIenv ->
         (let _lhsOconstFold :: Expr
              _lhsOcopy :: Expr
              _lhsOenv :: Env
              -- "ConstFold.ag"(line 131, column 17)
              _lhsOconstFold =
                  _copy
              -- self rule
              _copy =
                  Int val_
              -- self rule
              _lhsOcopy =
                  _copy
              -- copy rule (chain)
              _lhsOenv =
                  _lhsIenv
          in  ( _lhsOconstFold,_lhsOcopy,_lhsOenv)))
sem_Expr_Let :: T_TypeId  ->
                T_Expr  ->
                T_Expr  ->
                T_Expr 
sem_Expr_Let tyid_ sub_ body_  =
    (\ _lhsIenv ->
         (let _bodyOenv :: Env
              _lhsOconstFold :: Expr
              _lhsOcopy :: Expr
              _lhsOenv :: Env
              _tyidOenv :: Env
              _subOenv :: Env
              _tyidIargId :: Id
              _tyidIconstFoldF :: ((Id -> Expr -> Expr))
              _tyidIcopy :: TypeId
              _subIconstFold :: Expr
              _subIcopy :: Expr
              _subIenv :: Env
              _bodyIconstFold :: Expr
              _bodyIcopy :: Expr
              _bodyIenv :: Env
              -- "ConstFold.ag"(line 116, column 11)
              _sub' =
                  _subIconstFold
              -- "ConstFold.ag"(line 117, column 11)
              _bodyOenv =
                  M.add _tyidIargId _sub'     _lhsIenv
              -- "ConstFold.ag"(line 118, column 11)
              _body' =
                  _bodyIconstFold
              -- "ConstFold.ag"(line 119, column 11)
              _lhsOconstFold =
                  Let _tyidIcopy _sub'     _body'
              -- self rule
              _copy =
                  Let _tyidIcopy _subIcopy _bodyIcopy
              -- self rule
              _lhsOcopy =
                  _copy
              -- copy rule (up)
              _lhsOenv =
                  _bodyIenv
              -- copy rule (down)
              _tyidOenv =
                  _lhsIenv
              -- copy rule (down)
              _subOenv =
                  _lhsIenv
              ( _tyidIargId,_tyidIconstFoldF,_tyidIcopy) =
                  (tyid_ _tyidOenv )
              ( _subIconstFold,_subIcopy,_subIenv) =
                  (sub_ _subOenv )
              ( _bodyIconstFold,_bodyIcopy,_bodyIenv) =
                  (body_ _bodyOenv )
          in  ( _lhsOconstFold,_lhsOcopy,_lhsOenv)))
sem_Expr_LetRec :: T_Fundef  ->
                   T_Expr  ->
                   T_Expr 
sem_Expr_LetRec fundef_ body_  =
    (\ _lhsIenv ->
         (let _lhsOconstFold :: Expr
              _lhsOcopy :: Expr
              _lhsOenv :: Env
              _fundefOenv :: Env
              _bodyOenv :: Env
              _fundefIconstFold :: Fundef
              _fundefIcopy :: Fundef
              _fundefIenv :: Env
              _bodyIconstFold :: Expr
              _bodyIcopy :: Expr
              _bodyIenv :: Env
              -- "ConstFold.ag"(line 121, column 11)
              _lhsOconstFold =
                  LetRec _fundefIconstFold _bodyIconstFold
              -- self rule
              _copy =
                  LetRec _fundefIcopy _bodyIcopy
              -- self rule
              _lhsOcopy =
                  _copy
              -- copy rule (up)
              _lhsOenv =
                  _bodyIenv
              -- copy rule (down)
              _fundefOenv =
                  _lhsIenv
              -- copy rule (chain)
              _bodyOenv =
                  _fundefIenv
              ( _fundefIconstFold,_fundefIcopy,_fundefIenv) =
                  (fundef_ _fundefOenv )
              ( _bodyIconstFold,_bodyIcopy,_bodyIenv) =
                  (body_ _bodyOenv )
          in  ( _lhsOconstFold,_lhsOcopy,_lhsOenv)))
sem_Expr_LetTuple :: T_TypeIds  ->
                     Id ->
                     T_Expr  ->
                     T_Expr 
sem_Expr_LetTuple tyids_ ref_ body_  =
    (\ _lhsIenv ->
         (let _lhsOconstFold :: Expr
              _lhsOcopy :: Expr
              _lhsOenv :: Env
              _tyidsOenv :: Env
              _bodyOenv :: Env
              _tyidsIargId :: ([Id])
              _tyidsIconstFoldF :: ([(Id -> Expr -> Expr)])
              _tyidsIcopy :: TypeIds
              _bodyIconstFold :: Expr
              _bodyIcopy :: Expr
              _bodyIenv :: Env
              -- "ConstFold.ag"(line 124, column 11)
              _lhsOconstFold =
                  maybe (LetTuple _tyidsIcopy ref_ _bodyIconstFold)
                        (\xs -> foldleft2 (\e xt f -> f xt e)
                                          _bodyIconstFold
                                          xs
                                          _tyidsIconstFoldF)
                        (findTuple ref_ _lhsIenv)
              -- self rule
              _copy =
                  LetTuple _tyidsIcopy ref_ _bodyIcopy
              -- self rule
              _lhsOcopy =
                  _copy
              -- copy rule (up)
              _lhsOenv =
                  _bodyIenv
              -- copy rule (down)
              _tyidsOenv =
                  _lhsIenv
              -- copy rule (down)
              _bodyOenv =
                  _lhsIenv
              ( _tyidsIargId,_tyidsIconstFoldF,_tyidsIcopy) =
                  (tyids_ _tyidsOenv )
              ( _bodyIconstFold,_bodyIcopy,_bodyIenv) =
                  (body_ _bodyOenv )
          in  ( _lhsOconstFold,_lhsOcopy,_lhsOenv)))
sem_Expr_Neg :: Id ->
                T_Expr 
sem_Expr_Neg x_  =
    (\ _lhsIenv ->
         (let _lhsOconstFold :: Expr
              _lhsOcopy :: Expr
              _lhsOenv :: Env
              -- "ConstFold.ag"(line 70, column 11)
              _lhsOconstFold =
                  maybe _copy (Int . negate) (findInt x_ _lhsIenv)
              -- self rule
              _copy =
                  Neg x_
              -- self rule
              _lhsOcopy =
                  _copy
              -- copy rule (chain)
              _lhsOenv =
                  _lhsIenv
          in  ( _lhsOconstFold,_lhsOcopy,_lhsOenv)))
sem_Expr_Put :: Id ->
                Id ->
                Id ->
                T_Expr 
sem_Expr_Put aref_ iref_ vref_  =
    (\ _lhsIenv ->
         (let _lhsOconstFold :: Expr
              _lhsOcopy :: Expr
              _lhsOenv :: Env
              -- "ConstFold.ag"(line 131, column 17)
              _lhsOconstFold =
                  _copy
              -- self rule
              _copy =
                  Put aref_ iref_ vref_
              -- self rule
              _lhsOcopy =
                  _copy
              -- copy rule (chain)
              _lhsOenv =
                  _lhsIenv
          in  ( _lhsOconstFold,_lhsOcopy,_lhsOenv)))
sem_Expr_Sub :: Id ->
                Id ->
                T_Expr 
sem_Expr_Sub x_ y_  =
    (\ _lhsIenv ->
         (let _lhsOconstFold :: Expr
              _lhsOcopy :: Expr
              _lhsOenv :: Env
              -- "ConstFold.ag"(line 75, column 11)
              _lhsOconstFold =
                  maybe _copy
                        (Int . uncurry (-))
                        (both (findInt x_ _lhsIenv)
                              (findInt y_ _lhsIenv))
              -- self rule
              _copy =
                  Sub x_ y_
              -- self rule
              _lhsOcopy =
                  _copy
              -- copy rule (chain)
              _lhsOenv =
                  _lhsIenv
          in  ( _lhsOconstFold,_lhsOcopy,_lhsOenv)))
sem_Expr_Tuple :: ([Id]) ->
                  T_Expr 
sem_Expr_Tuple refs_  =
    (\ _lhsIenv ->
         (let _lhsOconstFold :: Expr
              _lhsOcopy :: Expr
              _lhsOenv :: Env
              -- "ConstFold.ag"(line 131, column 17)
              _lhsOconstFold =
                  _copy
              -- self rule
              _copy =
                  Tuple refs_
              -- self rule
              _lhsOcopy =
                  _copy
              -- copy rule (chain)
              _lhsOenv =
                  _lhsIenv
          in  ( _lhsOconstFold,_lhsOcopy,_lhsOenv)))
sem_Expr_Unit :: T_Expr 
sem_Expr_Unit  =
    (\ _lhsIenv ->
         (let _lhsOconstFold :: Expr
              _lhsOcopy :: Expr
              _lhsOenv :: Env
              -- "ConstFold.ag"(line 131, column 17)
              _lhsOconstFold =
                  _copy
              -- self rule
              _copy =
                  Unit
              -- self rule
              _lhsOcopy =
                  _copy
              -- copy rule (chain)
              _lhsOenv =
                  _lhsIenv
          in  ( _lhsOconstFold,_lhsOcopy,_lhsOenv)))
sem_Expr_Var :: Id ->
                T_Expr 
sem_Expr_Var ref_  =
    (\ _lhsIenv ->
         (let _lhsOconstFold :: Expr
              _lhsOcopy :: Expr
              _lhsOenv :: Env
              -- "ConstFold.ag"(line 69, column 11)
              _lhsOconstFold =
                  maybe _copy Int (findInt ref_ _lhsIenv)
              -- self rule
              _copy =
                  Var ref_
              -- self rule
              _lhsOcopy =
                  _copy
              -- copy rule (chain)
              _lhsOenv =
                  _lhsIenv
          in  ( _lhsOconstFold,_lhsOcopy,_lhsOenv)))
-- Fundef ------------------------------------------------------
{-
   visit 0:
      chained attribute:
         env                  : Env
      synthesized attributes:
         constFold            : Fundef
         copy                 : SELF 
   alternatives:
      alternative Fundef:
         child name           : TypeId 
         child args           : TypeIds 
         child body           : Expr 
         visit 0:
            local copy        : _
-}
-- cata
sem_Fundef :: Fundef  ->
              T_Fundef 
sem_Fundef (Fundef _name _args _body )  =
    (sem_Fundef_Fundef (sem_TypeId _name ) (sem_TypeIds _args ) (sem_Expr _body ) )
-- semantic domain
type T_Fundef  = Env ->
                 ( Fundef,Fundef,Env)
data Inh_Fundef  = Inh_Fundef {env_Inh_Fundef :: Env}
data Syn_Fundef  = Syn_Fundef {constFold_Syn_Fundef :: Fundef,copy_Syn_Fundef :: Fundef,env_Syn_Fundef :: Env}
wrap_Fundef :: T_Fundef  ->
               Inh_Fundef  ->
               Syn_Fundef 
wrap_Fundef sem (Inh_Fundef _lhsIenv )  =
    (let ( _lhsOconstFold,_lhsOcopy,_lhsOenv) =
             (sem _lhsIenv )
     in  (Syn_Fundef _lhsOconstFold _lhsOcopy _lhsOenv ))
sem_Fundef_Fundef :: T_TypeId  ->
                     T_TypeIds  ->
                     T_Expr  ->
                     T_Fundef 
sem_Fundef_Fundef name_ args_ body_  =
    (\ _lhsIenv ->
         (let _lhsOconstFold :: Fundef
              _lhsOcopy :: Fundef
              _lhsOenv :: Env
              _nameOenv :: Env
              _argsOenv :: Env
              _bodyOenv :: Env
              _nameIargId :: Id
              _nameIconstFoldF :: ((Id -> Expr -> Expr))
              _nameIcopy :: TypeId
              _argsIargId :: ([Id])
              _argsIconstFoldF :: ([(Id -> Expr -> Expr)])
              _argsIcopy :: TypeIds
              _bodyIconstFold :: Expr
              _bodyIcopy :: Expr
              _bodyIenv :: Env
              -- "ConstFold.ag"(line 150, column 11)
              _lhsOconstFold =
                  Fundef _nameIcopy _argsIcopy _bodyIconstFold
              -- self rule
              _copy =
                  Fundef _nameIcopy _argsIcopy _bodyIcopy
              -- self rule
              _lhsOcopy =
                  _copy
              -- copy rule (up)
              _lhsOenv =
                  _bodyIenv
              -- copy rule (down)
              _nameOenv =
                  _lhsIenv
              -- copy rule (down)
              _argsOenv =
                  _lhsIenv
              -- copy rule (down)
              _bodyOenv =
                  _lhsIenv
              ( _nameIargId,_nameIconstFoldF,_nameIcopy) =
                  (name_ _nameOenv )
              ( _argsIargId,_argsIconstFoldF,_argsIcopy) =
                  (args_ _argsOenv )
              ( _bodyIconstFold,_bodyIcopy,_bodyIenv) =
                  (body_ _bodyOenv )
          in  ( _lhsOconstFold,_lhsOcopy,_lhsOenv)))
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
      inherited attribute:
         env                  : Env
      synthesized attributes:
         argId                : Id
         constFoldF           : (Id -> Expr -> Expr)
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
type T_TypeId  = Env ->
                 ( Id,((Id -> Expr -> Expr)),TypeId)
data Inh_TypeId  = Inh_TypeId {env_Inh_TypeId :: Env}
data Syn_TypeId  = Syn_TypeId {argId_Syn_TypeId :: Id,constFoldF_Syn_TypeId :: (Id -> Expr -> Expr),copy_Syn_TypeId :: TypeId}
wrap_TypeId :: T_TypeId  ->
               Inh_TypeId  ->
               Syn_TypeId 
wrap_TypeId sem (Inh_TypeId _lhsIenv )  =
    (let ( _lhsOargId,_lhsOconstFoldF,_lhsOcopy) =
             (sem _lhsIenv )
     in  (Syn_TypeId _lhsOargId _lhsOconstFoldF _lhsOcopy ))
sem_TypeId_Tuple :: Id ->
                    T_Type  ->
                    T_TypeId 
sem_TypeId_Tuple argId_ argType_  =
    (\ _lhsIenv ->
         (let _lhsOconstFoldF :: ((Id -> Expr -> Expr))
              _lhsOargId :: Id
              _lhsOcopy :: TypeId
              _argTypeIcopy :: Type
              -- "ConstFold.ag"(line 143, column 13)
              _lhsOconstFoldF =
                  (\x e -> Let _copy (Var x) e)
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
          in  ( _lhsOargId,_lhsOconstFoldF,_lhsOcopy)))
-- TypeIds -----------------------------------------------------
{-
   visit 0:
      inherited attribute:
         env                  : Env
      synthesized attributes:
         argId                : [Id]
         constFoldF           : [(Id -> Expr -> Expr)]
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
type T_TypeIds  = Env ->
                  ( ([Id]),([(Id -> Expr -> Expr)]),TypeIds)
data Inh_TypeIds  = Inh_TypeIds {env_Inh_TypeIds :: Env}
data Syn_TypeIds  = Syn_TypeIds {argId_Syn_TypeIds :: [Id],constFoldF_Syn_TypeIds :: [(Id -> Expr -> Expr)],copy_Syn_TypeIds :: TypeIds}
wrap_TypeIds :: T_TypeIds  ->
                Inh_TypeIds  ->
                Syn_TypeIds 
wrap_TypeIds sem (Inh_TypeIds _lhsIenv )  =
    (let ( _lhsOargId,_lhsOconstFoldF,_lhsOcopy) =
             (sem _lhsIenv )
     in  (Syn_TypeIds _lhsOargId _lhsOconstFoldF _lhsOcopy ))
sem_TypeIds_Cons :: T_TypeId  ->
                    T_TypeIds  ->
                    T_TypeIds 
sem_TypeIds_Cons hd_ tl_  =
    (\ _lhsIenv ->
         (let _lhsOargId :: ([Id])
              _lhsOconstFoldF :: ([(Id -> Expr -> Expr)])
              _lhsOcopy :: TypeIds
              _hdOenv :: Env
              _tlOenv :: Env
              _hdIargId :: Id
              _hdIconstFoldF :: ((Id -> Expr -> Expr))
              _hdIcopy :: TypeId
              _tlIargId :: ([Id])
              _tlIconstFoldF :: ([(Id -> Expr -> Expr)])
              _tlIcopy :: TypeIds
              -- use rule "./TypeDEFS.ag"(line 28, column 32)
              _lhsOargId =
                  _hdIargId : _tlIargId
              -- use rule "ConstFold.ag"(line 138, column 31)
              _lhsOconstFoldF =
                  _hdIconstFoldF : _tlIconstFoldF
              -- self rule
              _copy =
                  (:) _hdIcopy _tlIcopy
              -- self rule
              _lhsOcopy =
                  _copy
              -- copy rule (down)
              _hdOenv =
                  _lhsIenv
              -- copy rule (down)
              _tlOenv =
                  _lhsIenv
              ( _hdIargId,_hdIconstFoldF,_hdIcopy) =
                  (hd_ _hdOenv )
              ( _tlIargId,_tlIconstFoldF,_tlIcopy) =
                  (tl_ _tlOenv )
          in  ( _lhsOargId,_lhsOconstFoldF,_lhsOcopy)))
sem_TypeIds_Nil :: T_TypeIds 
sem_TypeIds_Nil  =
    (\ _lhsIenv ->
         (let _lhsOargId :: ([Id])
              _lhsOconstFoldF :: ([(Id -> Expr -> Expr)])
              _lhsOcopy :: TypeIds
              -- use rule "./TypeDEFS.ag"(line 28, column 32)
              _lhsOargId =
                  []
              -- use rule "ConstFold.ag"(line 138, column 31)
              _lhsOconstFoldF =
                  []
              -- self rule
              _copy =
                  []
              -- self rule
              _lhsOcopy =
                  _copy
          in  ( _lhsOargId,_lhsOconstFoldF,_lhsOcopy)))
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