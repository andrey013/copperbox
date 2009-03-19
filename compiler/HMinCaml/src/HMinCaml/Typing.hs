

-- UUAGC 0.9.6 (Typing.ag)


-- |
-- Module: HMinCaml.Typing
-- License: as per original MinCaml
--
-- Maintainer: Stephen Tetley <stephen.tetley@gmail.com>
-- Stability: unstable
-- Portability: ghc
--
-- Type inference
--



module HMinCaml.Typing  where

import HMinCaml.CompilerMonad
import HMinCaml.Id
import HMinCaml.M
import HMinCaml.Syntax
import HMinCaml.Type

import Control.Applicative hiding ( empty )



typing :: Expr -> CM Expr
typing e = do
    -- ...
    derefTerm e

type Subst = M Id Type


unify :: Type -> Type -> (Subst,Type)
unify TUnit     TUnit                       = (empty,TUnit)
unify TBool     TBool                       = (empty,TBool)
unify TInt      TInt                        = (empty,TInt)
unify TFloat            TFloat              = (empty,TFloat)

unify (TVar a)          (TVar b)            | a == b  = (empty, TVar a)
unify (TVar (Just a))   b                   = unify a b
unify a                 (TVar (Just b))     = unify a b
unify (TVar Nothing)    b                   = if occur Nothing b  
                                                then undefined
                                                else undefined




occur :: Maybe Type -> Type -> Bool
occur r1 ty = occur_Syn_Type synthesized
  where
    synthesized = wrap_Type (sem_Type ty) inherited
    inherited = Inh_Type r1



derefTyp :: Type -> CM Type
derefTyp (TFun t1s t2)    = TFun    <$> (mapM derefTyp t1s) <*> derefTyp t2
derefTyp (TTuple ts)      = TTuple  <$> mapM derefTyp ts
derefTyp (TArray t)       = TArray  <$> derefTyp t
derefTyp (TVar Nothing)   = return $ TVar (Just TInt)
derefTyp (TVar (Just _t)) = error $ "do TVars need STRefs?"

derefTyp t                = return t
  
derefIdTyp :: (Id,Type) -> CM (Id,Type)
           
derefIdTyp (x, t) = (,) <$> pure x <*> derefTyp t

derefTerm :: Expr -> CM Expr
derefTerm (Not e) =  Not <$> derefTerm e

derefTerm _       = error $ "derefTerm todo"


    
         

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
    (let _argTypeOr1 :: ( Maybe Type )
         _argTypeIcopy :: Type
         _argTypeIoccur :: ( Bool )
         -- copy rule (chain)
         _argTypeOr1 =
             error "missing rule: LabeledType.Tuple.argType.r1"
         ( _argTypeIcopy,_argTypeIoccur) =
             (argType_ _argTypeOr1 )
     in  ( ))
-- OptType -----------------------------------------------------
{-
   visit 0:
      inherited attribute:
         r1                   :  Maybe Type 
      synthesized attributes:
         copy                 : SELF 
         occur                :  Bool 
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
type T_OptType  = ( Maybe Type ) ->
                  ( OptType,( Bool ))
data Inh_OptType  = Inh_OptType {r1_Inh_OptType ::  Maybe Type }
data Syn_OptType  = Syn_OptType {copy_Syn_OptType :: OptType,occur_Syn_OptType ::  Bool }
wrap_OptType :: T_OptType  ->
                Inh_OptType  ->
                Syn_OptType 
wrap_OptType sem (Inh_OptType _lhsIr1 )  =
    (let ( _lhsOcopy,_lhsOoccur) =
             (sem _lhsIr1 )
     in  (Syn_OptType _lhsOcopy _lhsOoccur ))
sem_OptType_Just :: T_Type  ->
                    T_OptType 
sem_OptType_Just just_  =
    (\ _lhsIr1 ->
         (let _lhsOoccur :: ( Bool )
              _lhsOcopy :: OptType
              _justOr1 :: ( Maybe Type )
              _justIcopy :: Type
              _justIoccur :: ( Bool )
              -- "Typing.ag"(line 69, column 17)
              _lhsOoccur =
                  _justIoccur
              -- self rule
              _copy =
                  Just _justIcopy
              -- self rule
              _lhsOcopy =
                  _copy
              -- copy rule (down)
              _justOr1 =
                  _lhsIr1
              ( _justIcopy,_justIoccur) =
                  (just_ _justOr1 )
          in  ( _lhsOcopy,_lhsOoccur)))
sem_OptType_Nothing :: T_OptType 
sem_OptType_Nothing  =
    (\ _lhsIr1 ->
         (let _lhsOoccur :: ( Bool )
              _lhsOcopy :: OptType
              -- "Typing.ag"(line 71, column 17)
              _lhsOoccur =
                  False
              -- self rule
              _copy =
                  Nothing
              -- self rule
              _lhsOcopy =
                  _copy
          in  ( _lhsOcopy,_lhsOoccur)))
-- Type --------------------------------------------------------
{-
   visit 0:
      inherited attribute:
         r1                   :  Maybe Type 
      synthesized attributes:
         copy                 : SELF 
         occur                :  Bool 
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
type T_Type  = ( Maybe Type ) ->
               ( Type,( Bool ))
data Inh_Type  = Inh_Type {r1_Inh_Type ::  Maybe Type }
data Syn_Type  = Syn_Type {copy_Syn_Type :: Type,occur_Syn_Type ::  Bool }
wrap_Type :: T_Type  ->
             Inh_Type  ->
             Syn_Type 
wrap_Type sem (Inh_Type _lhsIr1 )  =
    (let ( _lhsOcopy,_lhsOoccur) =
             (sem _lhsIr1 )
     in  (Syn_Type _lhsOcopy _lhsOoccur ))
sem_Type_TArray :: T_Type  ->
                   T_Type 
sem_Type_TArray ty_  =
    (\ _lhsIr1 ->
         (let _lhsOoccur :: ( Bool )
              _lhsOcopy :: Type
              _tyOr1 :: ( Maybe Type )
              _tyIcopy :: Type
              _tyIoccur :: ( Bool )
              -- "Typing.ag"(line 62, column 17)
              _lhsOoccur =
                  _tyIoccur
              -- self rule
              _copy =
                  TArray _tyIcopy
              -- self rule
              _lhsOcopy =
                  _copy
              -- copy rule (down)
              _tyOr1 =
                  _lhsIr1
              ( _tyIcopy,_tyIoccur) =
                  (ty_ _tyOr1 )
          in  ( _lhsOcopy,_lhsOoccur)))
sem_Type_TBool :: T_Type 
sem_Type_TBool  =
    (\ _lhsIr1 ->
         (let _lhsOoccur :: ( Bool )
              _lhsOcopy :: Type
              -- "Typing.ag"(line 65, column 17)
              _lhsOoccur =
                  False
              -- self rule
              _copy =
                  TBool
              -- self rule
              _lhsOcopy =
                  _copy
          in  ( _lhsOcopy,_lhsOoccur)))
sem_Type_TFloat :: T_Type 
sem_Type_TFloat  =
    (\ _lhsIr1 ->
         (let _lhsOoccur :: ( Bool )
              _lhsOcopy :: Type
              -- "Typing.ag"(line 65, column 17)
              _lhsOoccur =
                  False
              -- self rule
              _copy =
                  TFloat
              -- self rule
              _lhsOcopy =
                  _copy
          in  ( _lhsOcopy,_lhsOoccur)))
sem_Type_TFun :: T_Types  ->
                 T_Type  ->
                 T_Type 
sem_Type_TFun args_ retTy_  =
    (\ _lhsIr1 ->
         (let _lhsOoccur :: ( Bool )
              _argsOr1 :: ( Maybe Type )
              _retTyOr1 :: ( Maybe Type )
              _lhsOcopy :: Type
              _argsIcopy :: Types
              _argsIoccur :: ( [Bool] )
              _retTyIcopy :: Type
              _retTyIoccur :: ( Bool )
              -- "Typing.ag"(line 60, column 17)
              _lhsOoccur =
                  (or _argsIoccur) || _retTyIoccur
              -- "Typing.ag"(line 74, column 17)
              _argsOr1 =
                  _lhsIr1
              -- "Typing.ag"(line 75, column 17)
              _retTyOr1 =
                  _lhsIr1
              -- self rule
              _copy =
                  TFun _argsIcopy _retTyIcopy
              -- self rule
              _lhsOcopy =
                  _copy
              ( _argsIcopy,_argsIoccur) =
                  (args_ _argsOr1 )
              ( _retTyIcopy,_retTyIoccur) =
                  (retTy_ _retTyOr1 )
          in  ( _lhsOcopy,_lhsOoccur)))
sem_Type_TInt :: T_Type 
sem_Type_TInt  =
    (\ _lhsIr1 ->
         (let _lhsOoccur :: ( Bool )
              _lhsOcopy :: Type
              -- "Typing.ag"(line 65, column 17)
              _lhsOoccur =
                  False
              -- self rule
              _copy =
                  TInt
              -- self rule
              _lhsOcopy =
                  _copy
          in  ( _lhsOcopy,_lhsOoccur)))
sem_Type_TTuple :: T_Types  ->
                   T_Type 
sem_Type_TTuple tys_  =
    (\ _lhsIr1 ->
         (let _lhsOoccur :: ( Bool )
              _tysOr1 :: ( Maybe Type )
              _lhsOcopy :: Type
              _tysIcopy :: Types
              _tysIoccur :: ( [Bool] )
              -- "Typing.ag"(line 61, column 17)
              _lhsOoccur =
                  or _tysIoccur
              -- "Typing.ag"(line 76, column 17)
              _tysOr1 =
                  _lhsIr1
              -- self rule
              _copy =
                  TTuple _tysIcopy
              -- self rule
              _lhsOcopy =
                  _copy
              ( _tysIcopy,_tysIoccur) =
                  (tys_ _tysOr1 )
          in  ( _lhsOcopy,_lhsOoccur)))
sem_Type_TUnit :: T_Type 
sem_Type_TUnit  =
    (\ _lhsIr1 ->
         (let _lhsOoccur :: ( Bool )
              _lhsOcopy :: Type
              -- "Typing.ag"(line 65, column 17)
              _lhsOoccur =
                  False
              -- self rule
              _copy =
                  TUnit
              -- self rule
              _lhsOcopy =
                  _copy
          in  ( _lhsOcopy,_lhsOoccur)))
sem_Type_TVar :: T_OptType  ->
                 T_Type 
sem_Type_TVar optTy_  =
    (\ _lhsIr1 ->
         (let _lhsOoccur :: ( Bool )
              _optTyOr1 :: ( Maybe Type )
              _lhsOcopy :: Type
              _optTyIcopy :: OptType
              _optTyIoccur :: ( Bool )
              -- "Typing.ag"(line 63, column 17)
              _lhsOoccur =
                  _optTyIoccur
              -- "Typing.ag"(line 77, column 17)
              _optTyOr1 =
                  _lhsIr1
              -- self rule
              _copy =
                  TVar _optTyIcopy
              -- self rule
              _lhsOcopy =
                  _copy
              ( _optTyIcopy,_optTyIoccur) =
                  (optTy_ _optTyOr1 )
          in  ( _lhsOcopy,_lhsOoccur)))
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
         _argTypeOr1 :: ( Maybe Type )
         _argTypeIcopy :: Type
         _argTypeIoccur :: ( Bool )
         -- "./TypeDEFS.ag"(line 32, column 15)
         _lhsOargId =
             argId_
         -- self rule
         _copy =
             (argId_,_argTypeIcopy)
         -- self rule
         _lhsOcopy =
             _copy
         -- copy rule (chain)
         _argTypeOr1 =
             error "missing rule: TypeId.Tuple.argType.r1"
         ( _argTypeIcopy,_argTypeIoccur) =
             (argType_ _argTypeOr1 )
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
      inherited attribute:
         r1                   :  Maybe Type 
      synthesized attributes:
         copy                 : SELF 
         occur                :  [Bool] 
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
type T_Types  = ( Maybe Type ) ->
                ( Types,( [Bool] ))
data Inh_Types  = Inh_Types {r1_Inh_Types ::  Maybe Type }
data Syn_Types  = Syn_Types {copy_Syn_Types :: Types,occur_Syn_Types ::  [Bool] }
wrap_Types :: T_Types  ->
              Inh_Types  ->
              Syn_Types 
wrap_Types sem (Inh_Types _lhsIr1 )  =
    (let ( _lhsOcopy,_lhsOoccur) =
             (sem _lhsIr1 )
     in  (Syn_Types _lhsOcopy _lhsOoccur ))
sem_Types_Cons :: T_Type  ->
                  T_Types  ->
                  T_Types 
sem_Types_Cons hd_ tl_  =
    (\ _lhsIr1 ->
         (let _lhsOoccur :: ( [Bool] )
              _lhsOcopy :: Types
              _hdOr1 :: ( Maybe Type )
              _tlOr1 :: ( Maybe Type )
              _hdIcopy :: Type
              _hdIoccur :: ( Bool )
              _tlIcopy :: Types
              _tlIoccur :: ( [Bool] )
              -- use rule "Typing.ag"(line 56, column 44)
              _lhsOoccur =
                  _hdIoccur : _tlIoccur
              -- self rule
              _copy =
                  (:) _hdIcopy _tlIcopy
              -- self rule
              _lhsOcopy =
                  _copy
              -- copy rule (down)
              _hdOr1 =
                  _lhsIr1
              -- copy rule (down)
              _tlOr1 =
                  _lhsIr1
              ( _hdIcopy,_hdIoccur) =
                  (hd_ _hdOr1 )
              ( _tlIcopy,_tlIoccur) =
                  (tl_ _tlOr1 )
          in  ( _lhsOcopy,_lhsOoccur)))
sem_Types_Nil :: T_Types 
sem_Types_Nil  =
    (\ _lhsIr1 ->
         (let _lhsOoccur :: ( [Bool] )
              _lhsOcopy :: Types
              -- use rule "Typing.ag"(line 56, column 44)
              _lhsOoccur =
                  []
              -- self rule
              _copy =
                  []
              -- self rule
              _lhsOcopy =
                  _copy
          in  ( _lhsOcopy,_lhsOoccur)))