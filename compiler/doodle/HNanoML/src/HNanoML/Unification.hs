{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# OPTIONS -Wall #-}
-- |
-- Module      : HNanoML.Unification
-- License     : BSD-style (see the LICENSE file in the distribution)
-- Copyright   : 2009 Stephen Tetley
--
-- Maintainer  : Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   : unstable
-- Portability : ghc & uuag
--
-- Unification helpers for type inference
--


module HNanoML.Unification where

import HNanoML.Syntax ( Name )
import HNanoML.Type

import qualified Data.Map as Map
import Data.Monoid
import qualified Data.Set as Set



type TyError = String

newtype Subst = S { getSubst :: Map.Map TyId Type }


newtype TypeEnv = T { getTypeEnv :: Map.Map Name TypeScheme }

data TypeScheme  = TypeScheme [TyId] Type
                
                

instantiate :: TypeScheme -> Int -> (Type,Int)
instantiate (TypeScheme vars t) n = (s |=> t, n+length vars)
  where
    s = S . Map.fromList $ zip vars (map TyVar [n..]) 




element :: Name -> TypeEnv -> Maybe TypeScheme
element s (T env) = Map.lookup s env

addTE :: Name -> Type -> TypeEnv -> TypeEnv
addTE n t (T env) = T $ Map.insert n (scheme t []) env

(\\) :: TypeEnv -> Name -> TypeEnv
T a \\ n = T $ Map.delete n a    

union :: TypeEnv -> TypeEnv -> TypeEnv
union (T a) (T b) = T $ Map.union a b

env1 :: Name -> TypeScheme -> TypeEnv
env1 x b = T $ Map.singleton x b 

scheme :: Type -> [TyId] -> TypeScheme
scheme t xs = TypeScheme xs t


subst1 :: TyId -> Type -> Subst
subst1 = (S .) . Map.singleton


class Apply a where (|=>) :: Subst -> a -> a

instance Apply Type where
  s |=> (TyVar n)     = maybe (TyVar n) id (Map.lookup n $ getSubst s)
  s |=> (TyFun t1 t2) = TyFun (s |=> t1) (s |=> t2)
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
varBind n (TyVar u) | n == u      = (mempty, TyVar u)  
varBind n t     
    | n `Set.member` freevars t   = (mempty, TyError $ "occur check fails " 
                                                ++ show n ++ " " ++ show t)
    | otherwise                   = (subst1 n t, t)  
    


unify :: Type -> Type -> (Subst,Type)
unify (TyError s) _               = (mempty, TyError s)
unify _           (TyError s)     = (mempty, TyError s) 
unify (TyFun l r) (TyFun l' r')   = let (s1,t1) = unifylist l l'
                                        (s2,t2) = unify (s1 |=> r) (s1 |=> r')
                                    in (s1 `mappend` s2, TyFun t1 t2)    

unify (TyVar l)   (TyVar r) 
    | l == r                      = (mempty, TyVar r)
    | otherwise                   = (subst1 l (TyVar r), TyVar r)
unify l           (TyVar r)       = varBind r l
unify (TyVar l)   r               = varBind l r
unify TyInt       TyInt           = (mempty, TyInt)
unify TyBool      TyBool          = (mempty, TyBool)
unify l           r               = (mempty, TyError $ show l ++ 
                                                " does not unify with " ++ 
                                                show r)

unifylist :: [Type] -> [Type] -> (Subst,[Type])
unifylist as bs = step mempty [] as bs  where
  step s ts (x:xs) (y:ys) = let (s',t) = unify x y 
                            in case t of
                              TyError msg -> (mempty, [TyError msg])
                              _           -> step (s `mappend` s') (t:ts) xs ys
  step s ts []     []     = (s,reverse ts)
  step _ _  _      _      = (mempty, [TyError "TyFun - arglists do not unify"])

class Freevars a where freevars :: a -> Set.Set TyId

instance Freevars Type where
  freevars (TyVar name)       = Set.singleton name
  freevars (TyFun t1 t2)      = (freevars t1) `Set.union` (freevars t2)  
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
  
arrow :: Type -> Type -> Type
arrow a b = TyFun [a] b



unittype  :: Type
inttype   :: Type
booltype  :: Type

unittype  = TyUnit
inttype   = TyInt
booltype  = TyBool

int2unit  :: Type
int2unit = inttype `arrow` unittype

int2int2bool :: Type
int2int2bool = TyFun [inttype, inttype] booltype

int2int2int :: Type
int2int2int  = TyFun [inttype, inttype] inttype

int2int2unit :: Type
int2int2unit  = TyFun [inttype, inttype] unittype

initial_type_env :: TypeEnv
initial_type_env = T $ Map.fromList 
  [ ("print_int",       TypeScheme [] int2unit) 
  , ("print_int2",      TypeScheme [] int2int2unit) ]
  