{-# LANGUAGE FlexibleInstances          #-}


module WAlgo where

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
 
data PType = PType (Set.Set TyName) Type    
  deriving (Show)
  
type Subst = Map.Map TyName Type


runW :: Exp -> Type
runW e = (uncurry (|=>) . fst) $ runFresh (w (env0,e)) 0

-- (let id = \x -> x in id) :: t -> t
e0  =  Let "id" (Lam "x" (Var "x"))
        (Var "id")
-- (let id = \x -> x in id id) :: t -> t
e1  =  Let "id" (Lam "x" (Var "x"))
        (App (Var "id") (Var "id"))

-- (let id = \x -> let y = x in y in id id) :: t -> t
e2  =  Let "id" (Lam "x" (Let "y" (Var "x") (Var "y")))
        (App (Var "id") (Var "id"))

{-
e3  =  Let "id" (Lam "x" (Let "y" (Var "x") (Var "y")))
        (App (App (Var "id") (Var "id")) (ELit (LInt 2)))
-}

-- :t (let id = \x -> x x in id) - cannot construct the infinite type: t = t -> t1
e4  =  Let "id" (Lam "x" (App (Var "x") (Var "x")))
        (Var "id")

{-
-- (Bool -> a1) -> a1
e5  =  Lam "m" (Let "y" (Var "m")
                 (Let "x" (App (Var "y") (ELit (LBool True)))
                       (Var "x")))
-}                       
                       
        
w :: (TypeEnv,Exp) -> Fresh (Subst,Type)
w (te, Var x)       = (\t -> (mempty,t)) <$> instantiate (te &? x)
w (te, Lam x e)     = do xi     <- freshUnivar 
                         (s,t)  <- w (te `insT` (x,xi), e)
                         return (s, s |=> xi `arrow` t)
w (te, App e e')    = do (s,t)    <- w (te,e)
                         (s',t')  <- w (s |=> te, e')
                         xi       <- freshUnivar
                         let s''  = unify (s' |=> t) (t' `arrow` xi)
                         return (s'' `mappend` s' `mappend` s, s'' |=> xi)  
w (te, Let x e e')  = do (s,t)    <- w (te,e)
                         let si   = PType (gt t te) t
                         (s',t')  <- w (s |=> (te `insP` (x,si)),e')
                         return (s' `mappend` s, t') 

gt :: Type -> TypeEnv -> TypeNames
gt t te = ftv t Set.\\ ftv te

unify :: Type -> Type -> Subst
unify t             (UniVar n)    = unify (UniVar n) t
unify (UniVar n)    t            
    | not (n `Set.member` ftv t)  = Map.singleton n t 
    | otherwise                   = error $ "occur check fails" 
unify (TyFun t1 t2) (TyFun t3 t4) = let th = unify t1 t3 in
                                    (unify (th |=> t2) (th |=> t4)) `mappend` th
unify _             _             = mempty




-- W 
data TypeEnv = T (Map.Map Name (Either Type PType))
  deriving (Show)
  
env0 :: TypeEnv
env0 = (T mempty)

(&?) :: TypeEnv -> Name -> (Either Type PType)
(&?) (T env) x = maybe fk id $ Map.lookup x env where
    fk = error $ "Could not find " ++ x ++ " in the type env" 




insT :: TypeEnv -> (Name,Type) -> TypeEnv
insT (T env) (x,e)  = T (Map.insert x (Left e) env)

insP :: TypeEnv -> (Name,PType) -> TypeEnv
insP (T env) (x,s) = T  (Map.insert x (Right s) env)


  
type TypeNames = Set.Set TyName
 
class Ftv a where ftv :: a -> TypeNames

instance Ftv Type where
  ftv (UniVar name)      = Set.singleton name
  ftv (TyFun t1 t2)      = (ftv t1) `Set.union` (ftv t2)  
  ftv _                  = Set.empty

instance Ftv PType where
  ftv (PType vars t)  = (ftv t) Set.\\ vars

instance (Ftv a, Ftv b) => Ftv (Either a b) where
  ftv = either ftv ftv
  
instance Ftv TypeEnv where
  ftv (T env)  = Set.unions $ Map.fold (\e a -> ftv e : a) [] env 
  
instantiate (Left t)                = return t
instantiate (Right (PType alphs t)) = do 
    es <- count (Set.size alphs) freshUnivar
    let s = es // alphs 
    return (s |=> t)
    
(//) :: [Type] -> TypeNames -> Subst
(//) ts sn = Map.fromList $ zip (Set.toList sn) ts

freshUnivar :: Fresh Type
freshUnivar = UniVar <$> freshName

alphafresh :: Fresh Type
alphafresh = PolyVar <$> freshName

class Apply a where (|=>) :: Subst -> a -> a

instance Apply Type where
  s |=> (UniVar n)    = maybe (UniVar n) id (Map.lookup n s)
  s |=> (PolyVar n)   = maybe (PolyVar n) id (Map.lookup n s)  -- hmmm
  s |=> (TyFun t1 t2) = TyFun (s |=> t1) (s |=> t2)
  -- _ |=> t             = t 


instance Apply TypeEnv where
  s |=> (T env) = T $ Map.map (s |=>) env   

instance (Apply a, Apply b) => Apply (Either a b) where
  s |=> (Left a)  = Left $ s |=> a
  s |=> (Right b) = Right $ s |=> b   

instance Apply PType where
  s |=> (PType vars t) = PType vars t' where
      t' = s' |=> t
      s' = Set.fold (Map.delete) s vars




      
generalize :: PType -> Fresh Type
generalize (PType vars t) = (|=> t) <$> s
  where
    s    = (Map.fromList . zip (Set.toList vars) . map UniVar) <$> freshNames (Set.size vars) 
