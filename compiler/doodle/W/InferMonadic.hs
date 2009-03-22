
module InferMonadic where

import Unification
import Syntax

import Control.Monad.State
import qualified Data.Map as Map
import Data.Monoid


unboundErr :: String -> Type
unboundErr = TError . ("unbound variable " ++) 

newVar :: InferM Type
newVar = do 
    i <- gets uniq 
    modify $ \s -> s {uniq=i+1}
    return $ TVar i                                                       

isErr :: Type -> Bool
isErr (TError _)     = True
isErr _              = False  
  

data ISt = ISt { t_env ::  TypeEnv,  uniq :: Int }

type InferM a = State ISt a 

infer :: Exp -> InferM (Subst,Type)

infer (EVar var)             = do
    env <- gets t_env
    i   <- gets uniq
    case var `element` env of
      Nothing -> return (mempty, unboundErr var)
      Just s  -> do let (t,n) = instantiate s i
                    modify $ \s -> s { uniq=n }
                    return (mempty, t)

infer (ELit lit)              = inferLit lit                    

infer (EAbs var expr)         = do
    beta      <- newVar
    env       <- gets t_env
    modify $ \s -> s { t_env = (env \\ var) 
                                  `union` (env1 var (scheme beta [])) }
    (s1,t1)   <- infer expr
    return (s1,  (s1 |=> beta) `TFun` t1)                                 

infer (EApp fun arg)          = do
    (s1,t1)   <- infer fun
    env       <- gets t_env
    modify $ \s -> s { t_env = s1 |=> env }
    (s2,t2)   <- infer arg
    beta      <- newVar
    let (s3,t3) = unify (s2 |=> t1) (t2 `TFun` beta)
    if isErr t3 
      then return (s3,t3)
      else return (s3 `mappend` s2 `mappend` s1, s3 |=> beta)

infer (ELet var sub expr)     = do
    (s1,t1)   <- infer sub
    env       <- gets t_env
    let env'  = env1 var (generalize (s1 |=> env) t1)
    modify $ \s -> s { t_env = (s1 |=> env \\ var) `union` env' }
    (s2,t2)   <- infer expr
    return (s2 `mappend` s1, t2)
                         

inferLit :: Lit -> InferM (Subst,Type)
inferLit (LInt _)     = return (mempty,TInt)
inferLit (LBool _)    = return (mempty,TBool)


inferM :: Exp -> Type
inferM e = snd $ evalState (infer e) (ISt { t_env = T Map.empty, uniq = 0} )

