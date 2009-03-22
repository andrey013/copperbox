{-# LANGUAGE TypeSynonymInstances       #-}
{-# OPTIONS -Wall #-}

module Unification where

import Syntax

import qualified Data.Map as Map
import Data.Monoid

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


singleton :: VarName -> Type -> Subst
singleton = (S .) . Map.singleton

fromList :: [(VarName,Type)] -> Subst
fromList = S . Map.fromList


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


{-
unify :: Type -> Type -> Subst
unify (TError s)  _             = TError s
unify _           (TError s)    = TError s 
unify (TFun l r)  (TFun l' r')  = let s1 = unify l l'
                                      s2 = unify (s1 |=> r) (s1 |=> r')
                                  in s1 `mappend` s2    
unify (TVar l)  r               = varBind l r
unify l         (TVar r)        = varBind r l
unify TInt      TInt            = TInt
unify TBool     TBool           = TBool
unify l         r               = TError $ show l ++ " does not unify with "
                                                  ++ show r
                                    

varBind :: VarName -> Type -> Subst
varBind n t  
    | n `occurs` t    = (mempty, TError $ "occur check fails " 
                                   ++ show n ++ " " ++ show t)
    | otherwise       = (singleton n t, t)  
    
-}
  
varBind :: VarName -> Type -> (Subst,Type)
varBind n t  
    | n `occurs` t    = (mempty, TError $ "occur check fails " 
                                   ++ show n ++ " " ++ show t)
    | otherwise       = (singleton n t, t)  
    
occurs :: VarName -> Type -> Bool
occurs n (TVar u)     = n == u
occurs n (TFun t1 t2) = occurs n t1 || occurs n t2
occurs _ _            = False




unify :: Type -> Type -> (Subst,Type)
unify (TError s)  _             = (mempty, TError s)
unify _           (TError s)    = (mempty, TError s) 
unify (TFun l r)  (TFun l' r')  = let (s1,t1) = unify l l'
                                      (s2,t2) = unify (s1 |=> r) (s1 |=> r')
                                  in (s1 `mappend` s2, TFun t1 t2)    
unify (TVar l)    (TVar r) 
    | l == r                    = (mempty, TVar r)
    | otherwise                 = (singleton l (TVar r), TVar r)
unify l         (TVar r)        = varBind r l
unify (TVar l)  r               = varBind l r
unify TInt      TInt            = (mempty, TInt)
unify TBool     TBool           = (mempty, TBool)
unify l         r               = (mempty, TError $ 
                                    show l ++ " does not unify with " ++ show r)




