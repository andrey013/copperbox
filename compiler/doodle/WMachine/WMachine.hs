{-# LANGUAGE FlexibleInstances          #-}

-- NOT WORKING!

module WMachine where

import Fresh

import Control.Applicative
import qualified Data.Map as Map
import Data.Monoid
import qualified Data.Set as Set


-- Pure MiniML
type Name = String
type TyName = Int

data Exp = Var Name | Lam Name Exp | App Exp Exp | Let Name Exp Exp
  deriving (Eq,Show)
  
data Type = PolyVar Int | UniVar Int | TyFun Type Type
  deriving (Eq,Show)

arrow :: Type -> Type -> Type
arrow = TyFun  
 
data PType = PType [TyName] Type    
  deriving (Show)
  
type Subst = Map.Map TyName Type

-- W Machine
-- TypeEnv has a stack of names (so we can pop when required) and
-- a map from name to (type or poly-type) 
data TypeEnv = T [Name] (Map.Map Name (Either Type PType))
  deriving (Show)
  
env0 :: TypeEnv
env0 = (T [] mempty)

(&?) :: TypeEnv -> Name -> (Either Type PType)
(&?) (T _ env) x = maybe fk id $ Map.lookup x env where
    fk = error $ "Could not find " ++ x ++ " in the type env" 

popT :: TypeEnv -> (Type, TypeEnv)
popT (T (x:xs) env) = let t = maybe err (either id (const err)) $ 
                                Map.lookup x env  
                          err = error $ "Stack top not a monotype" 
                      in (t,T xs $ Map.delete x env)
popT (T []     _)   = error $ "Stack empty"

popP :: TypeEnv -> (PType, TypeEnv)
popP (T (x:xs) env) = let t = maybe err (either (const err) id) $ 
                                Map.lookup x env  
                          err = error $ "Stack top not a polytype" 
                      in (t,T xs $ Map.delete x env)
popP (T []     _)   = error $ "Stack empty"


pushT :: Name -> Type -> TypeEnv -> TypeEnv
pushT x e (T xs env) = T (x:xs) (Map.insert x (Left e) env)

pushP :: Name -> PType -> TypeEnv -> TypeEnv
pushP x s (T xs env) = T (x:xs) (Map.insert x (Right s) env)

-- NE (next expression), MT (monotype), abbreviated for the eval functions compactness 
data Control = NE Exp | MT Type
  deriving (Eq,Show)


data Frame = LamF | SqE Exp | SqT Type | LetInF Name Exp | LetF
  deriving (Eq,Show)
  
type TypeNames = Set.Set TyName
 
class Ftv a where ftv :: a -> TypeNames


instantiate (Left t)                = return t
instantiate (Right (PType alphs t)) = do 
    es <- count (length alphs) xifresh
    let s = es // alphs 
    return (s |=> t) where
    
(//) :: [Type] -> [Int] -> Subst
(//) = (Map.fromList .) . zipWith (flip (,))

xifresh :: Fresh Type
xifresh = UniVar <$> freshName

alphafresh :: Fresh Type
alphafresh = PolyVar <$> freshName

class Apply a where (|=>) :: Subst -> a -> a

instance Apply Type where
  s |=> (UniVar n)    = maybe (UniVar n) id (Map.lookup n s)
  s |=> (PolyVar n)   = maybe (PolyVar n) id (Map.lookup n s)  -- hmmm
  s |=> (TyFun t1 t2) = TyFun (s |=> t1) (s |=> t2)
  -- _ |=> t             = t 


instance Apply TypeEnv where
  s |=> (T stk m) = T stk $ Map.map (s |=>) m   

instance (Apply a, Apply b) => Apply (Either a b) where
  s |=> (Left a)  = Left $ s |=> a
  s |=> (Right b) = Right $ s |=> b   

instance Apply PType where
  s |=> (PType vars t) = PType vars t' where
      t' = s' |=> t
      s' = foldr (Map.delete) s vars

instance Apply Frame where
  s |=> LamF          = LamF 
  s |=> (SqE e)       = SqE e
  s |=> (SqT t)       = SqT $ s |=> t
  s |=> (LetInF x e)  = LetInF x e 
  s |=> LetF          = LetF 

instance Apply [Frame] where
  s |=> xs = map (s |=>) xs 
      
generalize :: PType -> Fresh Type
generalize (PType vars t) = (|=> t) <$> s
  where
    s    = (Map.fromList . zip vars . map UniVar) <$> freshNames (length vars) 
   
-- [var]
eval (NE (Var x),te,k)      = (\c -> (MT c,te,k)) <$> instantiate (te &? x)
-- [lam-in] 
eval (NE (Lam x e),te,k)    = (\xi -> (NE e,pushT x xi te,LamF:k)) <$> xifresh
-- [lam-out]
eval (MT t,te,LamF:k)       = (\(t',te') -> (MT $ t `arrow` t',te',k))
                                 <$> pure (popT te)
-- [app-l]
eval (NE (App e1 e2),te,k)  = pure (NE e1,te,SqE e2:k)                                  
-- [app-r]
eval (MT t,te,SqE e2:k)     = pure (NE e2,te,SqT t:k)                               
-- [app-out]
eval (MT t,te,SqT t':k)     = xifresh >>= \xi -> undefined xi >>= \s ->
                              return (MT $ s |=> xi,s |=> te,s |=> k)


eval a                      = error $ show a                      
                     
runW :: Fresh a -> a                          
runW f = fst $ runFresh f 0   


-- driver relies on fresh being a monad as well as applicative
driver :: Exp -> Fresh Control 
driver term = step (NE term, env0, []) where  
  step a = eval a >>= fn
  fn (val,_,[]) = return val
  fn t          = step t 


demo = runW $ driver e0


-- (let id = \x -> x in id) :: t -> t
e0  =  Let "id" (Lam "x" (Var "x"))
        (Var "id")
-- (let id = \x -> x in id id) :: t -> t
e1  =  Let "id" (Lam "x" (Var "x"))
        (App (Var "id") (Var "id"))

-- (let id = \x -> let y = x in y in id id) :: t -> t
e2  =  Let "id" (Lam "x" (Let "y" (Var "x") (Var "y")))
        (App (Var "id") (Var "id"))
        
        