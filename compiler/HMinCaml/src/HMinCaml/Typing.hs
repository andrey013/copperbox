{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  HMinCaml.Typing
-- Copyright   :  (c) Stephen Tetley 2012
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Type inference.
--
--------------------------------------------------------------------------------

module HMinCaml.Typing 
  ( 
    Env
  , ExtEnv
  , extEnv
  , lookupExtEnv

  , infer

  -- Temp...
  , Subst

  )  where

import qualified HMinCaml.HMonad as H
import HMinCaml.Id
import HMinCaml.Syntax
import HMinCaml.Type ( Type )
import qualified HMinCaml.Type as T

import Control.Applicative
import Control.Monad
import qualified Data.Foldable as F
import qualified Data.Map as M
import Data.Monoid
import Data.Set ( (\\) )
import qualified Data.Set as S

type ErrMsg = String

type Env = M.Map IdS Type

newtype ExtEnv = ExtEnv { getExtEnv :: M.Map IdS Type }

extEnv :: ExtEnv
extEnv = ExtEnv $ M.fromList [("print_int", T.Fun [T.Int] T.Unit)]


lookupExtEnv :: IdS -> ExtEnv -> Maybe T.Type
lookupExtEnv x = M.lookup x . getExtEnv

data St = St { count :: Int, global_phi :: Subst }

newtype Mon a = Mon { getMon :: H.HMonad ExtEnv () St a }


instance Functor Mon where
  fmap f = Mon . fmap f . getMon

instance Applicative Mon where
  pure a    = Mon $ pure a
  af <*> av = Mon $ getMon af <*> getMon av

instance Monad Mon where
  return    = pure
  ma >>= k  = Mon $ getMon ma >>= (getMon . k)

runMon :: Mon a -> Either String (a,Subst)
runMon ma = post $ H.runHMonad extEnv () st_zero (getMon ma)
  where
    st_zero = St { count      = 1
                 , global_phi = mempty } 

    post    = fmap (\(a,st) -> (a, global_phi st))


fatal :: String -> Mon a
fatal = Mon . H.fatal

freshi :: Mon Int
freshi = Mon $ H.puts $ \s -> let i = count s in (i, s { count = i + 1})

freshAlpha :: Mon T.Type
freshAlpha = fmap T.Var freshi 


findExternal :: IdS -> Mon (Maybe Type)
findExternal x = Mon $ H.ask1 >>= \env -> return (lookupExtEnv x env)


-- | During parsing we assign (-1) to type variables.
-- We need to translate them into fresh type variables.

whenUninstFresh :: T.Type -> Mon T.Type
whenUninstFresh (T.TypeLoc {}) = freshAlpha
whenUninstFresh t              = return t


whenUnistFreshArg :: (IdS,T.Type) -> Mon (IdS, T.Type)
whenUnistFreshArg (x,t) = (\ty -> (x,ty)) <$> whenUninstFresh t


type UnifyAns = Either ErrMsg Subst

-- phi is global name for the current substitution.
--

currentPhi :: Mon Subst
currentPhi = Mon $ H.gets global_phi



usePhi :: (Subst -> UnifyAns) -> (Subst -> Subst -> Subst) -> Mon ()
usePhi unifyk upd = 
   Mon $ H.gets global_phi >>= \phi -> 
         let ans = unifyk phi in case ans of 
             Left err -> H.fatal err
             Right s1 -> H.puts_ $ \s -> s { global_phi = s1 `upd` phi }


returnPhi :: (Subst -> a) -> Mon a
returnPhi retk = Mon $ H.gets global_phi >>= \phi -> return (retk phi)


type TyName = Int


data TypeScm = TypeScm (S.Set Int) T.Type
  deriving (Show)

newtype TypeEnv = TypeEnv { getTypeEnv :: M.Map IdS TypeScm }
  deriving (Show)

instance Monoid TypeEnv where
  mempty = TypeEnv mempty
  TypeEnv a `mappend` TypeEnv b = TypeEnv $ a `mappend` b


findTypeScm :: IdS -> TypeEnv -> Maybe TypeScm
findTypeScm x = M.lookup x . getTypeEnv

extTypeEnv :: IdS -> TypeScm -> TypeEnv -> TypeEnv
extTypeEnv x scm = TypeEnv . M.insert x scm . getTypeEnv

extTypeEnvs :: [(IdS,TypeScm)] -> TypeEnv -> TypeEnv
extTypeEnvs xs scm = foldr (\(x,t) m -> extTypeEnv x t m) scm xs

boostXi :: T.Type -> TypeScm
boostXi t@(T.Var {}) = TypeScm mempty t
boostXi _            = error $ "boostXi - not a T.Var"


class FreeTypeVars a where 
  ftv :: a -> S.Set TyName


instance FreeTypeVars T.Type where
  ftv (T.Unit)        = S.empty
  ftv (T.Bool)        = S.empty
  ftv (T.Int)         = S.empty
  ftv (T.Float)       = S.empty
  ftv (T.Fun ts t2)   = S.unions (map ftv ts) `S.union` (ftv t2)
  ftv (T.Tuple ts)    = S.unions (map ftv ts)
  ftv (T.Array t1)    = ftv t1
  ftv (T.Var i)       = S.singleton i
  ftv (T.TypeLoc {})  = S.empty   -- should be an error?


-- | Take the free variables of the type T, deleting the 
-- universally qualified variables.
--
instance FreeTypeVars TypeScm where
  ftv (TypeScm vars t)  = ftv t \\ vars


instance FreeTypeVars TypeEnv where
  ftv = S.unions . map ftv . M.elems . getTypeEnv

newtype Subst = Subst { getSubst :: M.Map TyName Type }
  deriving (Show)

makeSubst1 :: TyName -> Type -> Subst
makeSubst1 x ty = Subst $ M.singleton x ty

lookupSubst :: TyName -> Subst -> Maybe Type
lookupSubst x = M.lookup x . getSubst

instance Monoid Subst where
  mempty  = Subst mempty
  mappend = composeSubst


composeSubst :: Subst -> Subst -> Subst
composeSubst s1 s2 = applySubst s2 (getSubst s1)
  where
    applySubst s m = let initial =  M.map (s |=>) m  
                     in Subst $ initial `M.union` (getSubst s)

class ApplySubst a where
  (|=>) :: Subst -> a -> a

instance ApplySubst a => ApplySubst [a] where
  s |=> xs = map (s |=>) xs

instance ApplySubst T.Type where
  _ |=>   (T.Unit)       = T.Unit
  _ |=>   (T.Bool)       = T.Bool
  _ |=>   (T.Int)        = T.Int
  _ |=>   (T.Float)      = T.Float
  s |=>   (T.Fun ts t)   = T.Fun (map (s |=>) ts) (s |=> t)
  s |=>   (T.Tuple ts)   = T.Tuple (map (s |=>) ts)
  s |=>   (T.Array t)    = T.Array (s |=> t)
  s |=> t@(T.Var n)      = maybe t id $ lookupSubst n s  
  _ |=> t@(T.TypeLoc {}) = t

instance ApplySubst TypeScm where
  s |=> (TypeScm vars t) = TypeScm vars t' 
    where
      t' = s' |=> t
      s' = Subst $ F.foldr M.delete (getSubst s) vars

instance ApplySubst TypeEnv where
  s |=> (TypeEnv body) = TypeEnv $ M.map (s |=>) body




unify :: T.Type -> T.Type -> UnifyAns
unify (T.Var n)      t            
    | not (n `S.member` ftv t)      = Right $ makeSubst1 n t 
    | otherwise                     = Left $ "occur check fails" 

unify t              (T.Var n)      = unify (T.Var n) t

unify (T.Fun ts1 t2) (T.Fun ts3 t4) = do 
    s1 <- unifys ts1 ts3
    s2 <- unify (s1 |=> t2) (s1 |=> t4)
    return (s2 `mappend` s1)


unify (T.Fun {})     t              = Left $ "not Fun: " ++ show (T.ppType t)
unify t              (T.Fun {})     = Left $ "not Fun: " ++ show (T.ppType t)

unify (T.Unit)       (T.Unit)       = Right mempty
unify (T.Unit)       t              = Left $ "not Unit: " ++ show (T.ppType t)
unify t              (T.Unit)       = Left $ "not Unit: " ++ show (T.ppType t)

unify (T.Bool)       (T.Bool)       = Right mempty
unify (T.Bool)       t              = Left $ "not Bool: " ++ show (T.ppType t)
unify t              (T.Bool)       = Left $ "not Bool: " ++ show (T.ppType t)

unify (T.Int)        (T.Int)        = Right mempty
unify (T.Int)        t              = Left $ "not Int: " ++ show (T.ppType t)
unify t              (T.Int)        = Left $ "not Int: " ++ show (T.ppType t)

unify (T.Float)      (T.Float)      = Right mempty
unify (T.Float)      t              = Left $ "not Float: " ++ show (T.ppType t)
unify t              (T.Float)      = error $ "not Float: " ++ show (T.ppType t)

unify (T.Array t1)   (T.Array t2)   = unify t1 t2
unify (T.Array {})   t              = Left $ "not Array: " ++ show (T.ppType t)
unify t              (T.Array {})   = Left $ "not Array: " ++ show (T.ppType t)

unify (T.Tuple ts1)  (T.Tuple ts2)  = unifys ts1 ts2

unify (T.TypeLoc {}) _              = Left $ "cannot unify a TypeLoc"
unify _              (T.TypeLoc {}) = Left $ "cannot unify a TypeLoc"


unifys :: [T.Type] -> [T.Type] -> UnifyAns
unifys = go mempty
  where
    go ac (t1:t1s) (t2:t2s)  = case unify t1 t2 of
                                 Left err -> Left err
                                 Right s1  -> go (ac `mappend` s1) t1s t2s

    go ac []       []        = Right ac
    go _  _        _         = Left $ "unify - mis-matched arg lists"


-- @instantiate@ - replace the qualified variables by fresh type 
-- variables.
--
instantiate :: TypeScm -> Mon Type
instantiate (TypeScm vars t) = do
    alphs <- replicateM (S.size vars) freshAlpha
    return (mksubst alphs |=> t)
  where
    mksubst alphs = Subst $ M.fromList $ zip (S.toList vars) alphs


generalize :: TypeEnv -> Type -> TypeScm
generalize gam t = TypeScm (ftv t \\ ftv gam) t

makePolytype :: TypeEnv -> Type -> Mon TypeScm
makePolytype gam t1 =
    (\phi  -> generalize (phi |=> gam) (phi |=> t1)) <$> currentPhi

-- | Algorithm J adapt to perform type based translation.
--
transJ :: TypeEnv -> Expr -> Mon (T.Type, Expr)
transJ _   (Unit)               = return (T.Unit, Unit)

transJ _   (Bool x)             = return (T.Bool, Bool x)

transJ _   (Int x)              = return (T.Int, Int x)

transJ _   (Float x)            = return (T.Float, Float x)

transJ gam (Not e)              = do
    (t1,e1)  <- transJ gam e
    usePhi (\s -> unify T.Bool (s |=> t1)) mappend
    return (T.Bool, Not $ e1)

transJ gam (Neg e)              = do
    (t1, e1)   <- transJ gam e
    usePhi (\s -> unify T.Int (s |=> t1)) mappend
    return (T.Int, e1)

transJ gam (Add e1 e2)         = binaryPrim gam T.Int Add e1 e2
transJ gam (Sub e1 e2)         = binaryPrim gam T.Int Sub e1 e2


transJ gam (FNeg e)             = do
    (t1,e1) <- transJ gam e
    usePhi (\s -> unify T.Float (s |=> t1)) mappend
    return (T.Float, FNeg e1)

transJ gam (FAdd e1 e2)         = binaryPrim gam T.Float FAdd e1 e2
transJ gam (FSub e1 e2)         = binaryPrim gam T.Float FSub e1 e2
transJ gam (FMul e1 e2)         = binaryPrim gam T.Float FMul e1 e2
transJ gam (FDiv e1 e2)         = binaryPrim gam T.Float FDiv e1 e2


transJ gam (Eq e1 e2)           = do
    (t1,ea) <- transJ gam e1
    (t2,eb) <- transJ gam e2
    usePhi (\s -> unify (s |=> t1) (s |=> t2)) mappend
    return (T.Bool, Eq ea eb)


transJ gam (LE e1 e2)          = do
    (t1,ea) <- transJ gam e1
    (t2,eb) <- transJ gam e2
    usePhi (\s -> unify (s |=> t1) (s |=> t2)) mappend
    return (T.Bool, LE ea eb)

transJ gam (If e1 e2 e3)       = do
    (t1,ea) <- transJ gam e1
    usePhi (\s -> unify T.Bool (s |=> t1)) mappend
    (t2,eb) <- transJ gam e2
    (t3,ec) <- transJ gam e3
    usePhi (\s -> unify (s |=> t2) (s |=> t3)) mappend
    return (t3, If ea eb ec)


-- TODO treat _t0 (TypeLoc) ...
transJ gam (Let (x,_t0) e1 e2)  = do
    (t1,ea)  <- transJ gam e1
    sigma    <- makePolytype gam t1
    (t2,eb)  <- transJ (extTypeEnv x sigma gam) e2
    return (t2, Let (x,t2) ea eb)

transJ gam (Var x)               = 
    case x `findTypeScm` gam of 
       Just sc -> instantiate sc >>= \t -> return (t, Var x)
       Nothing -> findExternal x >>= maybe fk (\t -> return (t, Var x))
  where
    fk = fatal $ "unbound variable " ++ x

-- This is a bit dodgy...
transJ gam (LetRec (Fundef (x,t0) args0 e1) e2) = do
    t         <- whenUninstFresh t0
    args      <- mapM whenUnistFreshArg args0
    let gam1 = extTypeEnv x (boostXi t) gam
    (t1,ea)   <- transJ (extTypeEnvs (boostArgs args) gam1) e1
    usePhi (\s -> unify (s |=> t) (s |=> T.Fun (map snd args) t1)) mappend
    (t2,eb)   <- transJ gam1 e2
    returnPhi $ \s -> let fhead = (x,funType s t2 args)
                      in (t2, LetRec (Fundef fhead (substArgs s args) ea) eb)
  where
    boostArgs = map (\(v,ty) -> (v, boostXi ty))
    substArgs = \s args -> map (\(a,b) -> (a, s |=> b)) args
    funType   = \s t args -> T.Fun (map snd $ substArgs s args) (s |=> t)

transJ gam (App e es)           = do
    (t1,e1)   <- transJ gam e
    (ts2,es1) <- fmap unzip $ transJs gam es
    alpha   <- freshAlpha
    usePhi (\s ->  unify (s |=> t1) (s |=> ts2 `T.Fun` alpha)) mappend
    returnPhi $ \s -> (s |=> alpha, App e1 es1)

-- NOTE - this looks dubious...
transJ gam (Tuple es)           = do
    (ts,es1) <- unzip <$> transJs gam es
    as <- replicateM (length es) $ freshAlpha
    usePhi (\s -> unifys (s |=> ts) (s |=> as)) mappend
    returnPhi $ \s -> (T.Tuple (s |=> ts), Tuple es1)

transJ _gam (LetTuple _xts _e1 _e2)  = do
    error $ "LetTuple"

transJ gam (Array e1 e2)        = do
    (t1,ea)  <- transJ gam e1
    alpha1   <- freshAlpha
    usePhi (\s -> unify (s |=> t1) (s |=> T.Array alpha1)) mappend
    (t2,eb)  <- transJ gam e2
    usePhi (\s -> unify (s |=> t2) T.Int) mappend
    returnPhi $ \s -> (s |=> alpha1, Array ea eb)

transJ gam (Get e1 eix)          = do
    (t1,ea)  <- transJ gam e1
    alpha1   <- freshAlpha
    usePhi (\s -> unify (s |=> t1) (s |=> T.Array alpha1)) mappend
    (t2,eb)  <- transJ gam eix
    usePhi (\s -> unify (s |=> t2) T.Int) mappend
    returnPhi $ \s -> (s |=> alpha1, Get ea eb)

transJ gam (Put e1 eix e3)       = do
    (t1,ea)  <- transJ gam e1
    alpha1   <- freshAlpha
    usePhi (\s -> unify (s |=> t1) (s |=> T.Array alpha1)) mappend
    (t2,eb)  <- transJ gam eix
    usePhi (\s -> unify (s |=> t2) T.Int) mappend
    (t3,ec)  <- transJ gam e3
    usePhi (\s -> unify (s |=> t1) (s |=> T.Array t3)) mappend
    return (T.Unit, Put ea eb ec)



transJs :: TypeEnv -> [Expr] -> Mon [(T.Type, Expr)]
transJs _   []       = return []
transJs gam (e:es)   = (:) <$> transJ gam e <*> transJs gam es



binaryPrim :: TypeEnv -> T.Type -> (Expr -> Expr -> Expr) 
           -> Expr -> Expr -> Mon (T.Type, Expr)
binaryPrim gam ty_assert build e1 e2 = do
    (t1,ea) <- transJ gam e1
    (t2,eb) <- transJ gam e2
    usePhi (\s -> unify ty_assert (s |=> t1)) mappend
    usePhi (\s -> unify ty_assert (s |=> t2)) mappend
    return (ty_assert, build ea eb)


infer :: Expr -> Either String Expr
infer e = fmap post $ runMon $ transJ mempty e
  where
    post ((_,e1),_) = e1 

