{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# OPTIONS -Wall #-}
-- |
-- Module: HMinCaml.Unification
-- License: as per original MinCaml
-- Copyright 2009 Stephen Tetley
--
-- Maintainer: Stephen Tetley <stephen.tetley@gmail.com>
-- Stability: unstable
-- Portability: ghc
--
-- Unification helpers for type inference
--


module HMinCaml.Unification where

import HMinCaml.Id
import HMinCaml.Type

import qualified Data.Map as Map
import Data.Monoid
import qualified Data.Set as Set



type TyError = String

newtype Subst = S { getSubst :: Map.Map TyId Type }


newtype TypeEnv = T { getTypeEnv :: Map.Map Id TypeScheme }

data TypeScheme  = TypeScheme [TyId] Type
                
                

instantiate :: TypeScheme -> Int -> (Type,Int)
instantiate (TypeScheme vars t) n = (s |=> t, n+length vars)
  where
    s = S . Map.fromList $ zip vars (map TVar [n..]) 




element :: Id -> TypeEnv -> Maybe TypeScheme
element s (T env) = Map.lookup s env


(\\) :: TypeEnv -> Id -> TypeEnv
T a \\ n = T $ Map.delete n a    

union :: TypeEnv -> TypeEnv -> TypeEnv
union (T a) (T b) = T $ Map.union a b

env1 :: Id -> TypeScheme -> TypeEnv
env1 x b = T $ Map.singleton x b 

scheme :: Type -> [TyId] -> TypeScheme
scheme t xs = TypeScheme xs t


subst1 :: TyId -> Type -> Subst
subst1 = (S .) . Map.singleton


class Apply a where (|=>) :: Subst -> a -> a

instance Apply Type where
  s |=> (TVar n)      = maybe (TVar n) id (Map.lookup n $ getSubst s)
  s |=> (TFun t1 t2)  = TFun (s |=> t1) (s |=> t2)
  _ |=> t             = t  

instance Apply [Type] where
  s |=> xs = map (s |=>) xs

instance Apply TypeScheme where
  s |=> (TypeScheme vars t) = TypeScheme vars t' where
      t' = s' |=> t
      s' = S $ foldr (Map.delete) (getSubst s) vars

instance Apply TypeEnv where
  s |=> (T m) = T $ Map.map (s |=>) m      

instance Monoid Subst where
  mempty = S mempty
  S s1 `mappend` S s2 = S $ s1 `mappend` fmap ((S s1) |=>) s2

                         
  
varBind :: TyId -> Type -> (Subst,Type)
varBind n (TVar u) | n == u       = (mempty, TVar u)  
varBind n t     
    | n `Set.member` freevars t   = (mempty, TError $ "occur check fails " 
                                                ++ show n ++ " " ++ show t)
    | otherwise                   = (subst1 n t, t)  
    


unify :: Type -> Type -> (Subst,Type)
unify (TError s)  _             = (mempty, TError s)
unify _           (TError s)    = (mempty, TError s) 
{-
unify (TFun l r)  (TFun l' r')  = let (s1,t1) = unify l l'
                                      (s2,t2) = unify (s1 |=> r) (s1 |=> r')
                                  in (s1 `mappend` s2, TFun t1 t2)    
-}
unify (TVar l)    (TVar r) 
    | l == r                    = (mempty, TVar r)
    | otherwise                 = (subst1 l (TVar r), TVar r)
unify l         (TVar r)        = varBind r l
unify (TVar l)  r               = varBind l r
unify TInt      TInt            = (mempty, TInt)
unify TBool     TBool           = (mempty, TBool)
unify l         r               = (mempty, TError $ 
                                    show l ++ " does not unify with " ++ show r)

class Freevars a where freevars :: a -> Set.Set TyId

instance Freevars Type where
  freevars (TVar name)        = Set.singleton name
  freevars (TFun t1 t2)       = (freevars t1) `Set.union` (freevars t2)  
  freevars _                  = Set.empty


instance Freevars [Type] where
  freevars xs = foldr (Set.union) mempty (map freevars xs)
                
instance Freevars TypeScheme where
  freevars (TypeScheme vars t)  = foldr (Set.delete) (freevars t) vars
  
instance Freevars TypeEnv where
  freevars (T env)  = Set.unions $ Map.fold (\e a -> freevars e : a) [] env 


generalize :: TypeEnv -> Type -> TypeScheme
generalize te t = TypeScheme vars t where
  vars = Set.toList $ freevars t `Set.difference` freevars te  
  
arrow1 :: Type -> Type -> Type
arrow1 a b = TFun [a] b

arrow2 :: Type -> Type -> Type -> Type
arrow2 a b c = TFun [a,b] c

unittype  :: Type
inttype   :: Type
floattype :: Type
booltype  :: Type

unittype  = TUnit
inttype   = TInt
floattype = TFloat
booltype  = TBool

int2unit  :: Type
int2unit = arrow1 inttype unittype

int2int2bool :: Type
int2int2bool = arrow2 inttype inttype booltype

int2int2int :: Type
int2int2int = arrow2 inttype inttype inttype


initial_type_env :: TypeEnv
initial_type_env = T $ Map.fromList 
  [ ("print_int", TypeScheme [] int2unit) ]