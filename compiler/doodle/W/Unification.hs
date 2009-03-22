{-# LANGUAGE TypeSynonymInstances       #-}
{-# OPTIONS -Wall #-}

module Unification where

import Syntax

import qualified Data.Map as Map
import Data.Monoid
import qualified Data.Set as Set


  
type TyError = String

newtype Subst = S { getSubst :: Map.Map VarName Type }


newtype TypeEnv = T { getTypeEnv :: Map.Map EName TypeScheme }


instantiate :: TypeScheme -> Int -> (Type,Int)
instantiate (TypeScheme vars t) n = (s |=> t, n+length vars)
  where
    s = S . Map.fromList $ zip vars (map TVar [n..]) 




element :: EName -> TypeEnv -> Maybe TypeScheme
element s (T env) = Map.lookup s env


(\\) :: TypeEnv -> EName -> TypeEnv
T a \\ n = T $ Map.delete n a    

union :: TypeEnv -> TypeEnv -> TypeEnv
union (T a) (T b) = T $ Map.union a b

env1 :: EName -> TypeScheme -> TypeEnv
env1 x b = T $ Map.singleton x b 

scheme :: Type -> [VarName] -> TypeScheme
scheme t xs = TypeScheme xs t


subst1 :: VarName -> Type -> Subst
subst1 = (S .) . Map.singleton


class Apply a where (|=>) :: Subst -> a -> a

instance Apply Type where
  s |=> (TVar n)      = maybe (TVar n) id (Map.lookup n $ getSubst s)
  s |=> (TFun t1 t2)  = TFun (s |=> t1) (s |=> t2)
  _ |=> t             = t  

instance Apply TypeScheme where
  s |=> (TypeScheme vars t) = TypeScheme vars t' where
      t' = s' |=> t
      s' = S $ foldr (Map.delete) (getSubst s) vars

instance Apply TypeEnv where
  s |=> (T m) = T $ Map.map (s |=>) m      

instance Monoid Subst where
  mempty = S mempty
  S s1 `mappend` S s2 = S $ s1 `mappend` fmap ((S s1) |=>) s2

                         
  
varBind :: VarName -> Type -> (Subst,Type)
varBind n (TVar u) | n == u       = (mempty, TVar u)  
varBind n t     
    | n `Set.member` freevars t   = (mempty, TError $ "occur check fails " 
                                                ++ show n ++ " " ++ show t)
    | otherwise                   = (subst1 n t, t)  
    


unify :: Type -> Type -> (Subst,Type)
unify (TError s)  _             = (mempty, TError s)
unify _           (TError s)    = (mempty, TError s) 
unify (TFun l r)  (TFun l' r')  = let (s1,t1) = unify l l'
                                      (s2,t2) = unify (s1 |=> r) (s1 |=> r')
                                  in (s1 `mappend` s2, TFun t1 t2)    
unify (TVar l)    (TVar r) 
    | l == r                    = (mempty, TVar r)
    | otherwise                 = (subst1 l (TVar r), TVar r)
unify l         (TVar r)        = varBind r l
unify (TVar l)  r               = varBind l r
unify TInt      TInt            = (mempty, TInt)
unify TBool     TBool           = (mempty, TBool)
unify l         r               = (mempty, TError $ 
                                    show l ++ " does not unify with " ++ show r)

class Freevars a where freevars :: a -> Set.Set VarName

instance Freevars Type where
  freevars (TVar name)        = Set.singleton name
  freevars (TFun t1 t2)       = (freevars t1) `Set.union` (freevars t2)  
  freevars _                  = Set.empty

                
instance Freevars TypeScheme where
  freevars (TypeScheme vars t)  = foldr (Set.delete) (freevars t) vars
  
instance Freevars TypeEnv where
  freevars (T env)  = Set.unions $ Map.fold (\e a -> freevars e : a) [] env 


generalize :: TypeEnv -> Type -> TypeScheme
generalize te t = TypeScheme vars t where
  vars = Set.toList $ freevars t `Set.difference` freevars te  
  


