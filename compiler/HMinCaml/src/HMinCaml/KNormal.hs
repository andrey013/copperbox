{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  HMinCaml.KNormal
-- Copyright   :  (c) Stephen Tetley 2012
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC (MPTC and more)
--
-- K-normal form.
--
--------------------------------------------------------------------------------

module HMinCaml.KNormal 
  (
    Expr(..)


    -- Re-export fundef from Syntax
  , Fundef(..)

  , ppExpr

  , freevars

  , knormal

  ) where

import HMinCaml.HMonad  ( (<<|>) )
import qualified HMinCaml.HMonad as H
import HMinCaml.Id
import qualified HMinCaml.Syntax as S
import qualified HMinCaml.Type as T
import HMinCaml.Typing ( ExtEnv, extEnv, lookupExtEnv )

import Text.PrettyPrint.HughesPJ.PrettyExpr.Ocaml   -- package: hpj-pretty-expr


import Text.PrettyPrint.HughesPJ

import Control.Applicative
import Control.Monad
import Data.List ( intersperse )
import qualified Data.Map as M
import qualified Data.Set as S




data Expr = Unit 
          | Int Int
          | Float Double
          | Neg IdS
          | Add IdS IdS
          | Sub IdS IdS
          | FNeg IdS
          | FAdd IdS IdS
          | FSub IdS IdS
          | FMul IdS IdS
          | FDiv IdS IdS
          | IfEq IdS IdS Expr Expr
          | IfLE IdS IdS Expr Expr
          | Let (IdS,T.Type) Expr Expr
          | Var IdS
          | LetRec Fundef Expr
          | App IdS [IdS]
          | Tuple [IdS]
          | LetTuple [(IdS, T.Type)] IdS Expr
          | Get IdS IdS
          | Put IdS IdS IdS
          | ExtArray IdS
          | ExtFunApp IdS [IdS]
  deriving (Eq,Show)

-- Fundef is the same as Syntax, except for Expr...

data Fundef = Fundef 
    { fun_head       :: (IdS, T.Type)
    , fun_args       :: [(IdS, T.Type)]
    , fun_body       :: Expr
    } 
  deriving (Eq, Show)



ppExpr :: Expr -> Doc
ppExpr = unparse . exprDoc



exprDoc :: Expr -> DocE
exprDoc Unit                 = literal "()"
exprDoc (Int i)              = Atom $ int i 
exprDoc (Float d)            = Atom $ double $ realToFrac d 
exprDoc (Neg x)              = inegateU $ literal x 
exprDoc (Add x y)            = iaddB (literal x) (literal y)
exprDoc (Sub x y)            = isubtractB (literal x) (literal y)
exprDoc (FNeg x)             = fnegateU $ literal x 
exprDoc (FAdd x y)           = faddB (literal x) (literal y)
exprDoc (FSub x y)           = fsubtractB (literal x) (literal y)
exprDoc (FMul x y)           = fmultiplyB (literal x) (literal y)
exprDoc (FDiv x y)           = fdivideB (literal x) (literal y)
exprDoc (IfEq x y e1 e2)     = 
    ppIf (structuralEqB (literal x) (literal y)) e1 e2

exprDoc (IfLE x y e1 e2)     = 
    ppIf (lessThanEqB (literal x) (literal y)) e1 e2


exprDoc (Let (v,t) e b)      = ppLet v t e b

exprDoc (Var s)              = literal s
exprDoc (LetRec fd e)        = 
    Atom $ ppFundef fd <+> text "in" <+> ppExpr e

exprDoc (App x xs)           = funAppB (literal x) (map literal xs)

exprDoc (Tuple xs)           = 
    Atom $ parens $ hcat $ intersperse comma $ map text xs

exprDoc (LetTuple xts x e)   = 
    Atom $ text "let" <+> ppTupleVars xts <+> char '=' <+> text x
                      <+> text "in" <+> ppExpr e

exprDoc (Get x y)          = 
    Atom $ text x <> char '.' <> parens (text y)

exprDoc (Put x y z)        = 
    Atom $ text x <> char '.' <> parens (text y) 
                  <+> text "<-" <+> text z

exprDoc (ExtArray x)       = 
    Atom $ text "ext_array" <+> text x

exprDoc (ExtFunApp x xs)   = 
    Atom $ text ("ext." ++ x) <+> hsep (map text xs)

ppLet :: String -> T.Type -> Expr -> Expr -> DocE
ppLet v ty e b = 
    Atom $ text "let" <+> text v <+> text "::" <+> T.ppType ty
                      <+> char '=' <+> ppExpr e <+> text "in" 
                      $+$ nest 2 (ppExpr b)

ppFundef :: Fundef -> Doc
ppFundef (Fundef hd args e) = 
    text "let rec" <+> text (fst hd) <+> hsep (map (text .fst) args) <+> char '=' 
      $+$ nest 2 (ppExpr e)

ppTupleVars :: [(IdS, T.Type)] -> Doc
ppTupleVars = parens . hcat . intersperse comma . map (text . fst)


ppIf :: DocE -> Expr -> Expr -> DocE
ppIf clause e1 e2 = 
    Atom $ text "if" <+> unparse clause 
                     $+$ nest 2 (text "then" <+> ppExpr e1)
                     $+$ nest 2 (text "else" <+> ppExpr e2)




--------------------------------------------------------------------------------
-- Free variables

freevars :: Expr -> S.Set IdS
freevars Unit                 = S.empty
freevars (Int {})             = S.empty
freevars (Float {})           = S.empty
freevars (Neg x)              = S.singleton x
freevars (Add x y)            = S.fromList [x,y]
freevars (Sub x y)            = S.fromList [x,y]
freevars (FNeg x)             = S.singleton x
freevars (FAdd x y)           = S.fromList [x,y]
freevars (FSub x y)           = S.fromList [x,y]
freevars (FMul x y)           = S.fromList [x,y]
freevars (FDiv x y)           = S.fromList [x,y]
freevars (IfEq x y e1 e2)     = 
    S.insert x $ S.insert y $ S.union (freevars e1) (freevars e2)

freevars (IfLE x y e1 e2)     = 
    S.insert x $ S.insert y $ S.union (freevars e1) (freevars e2)

freevars (Let (x,_) e1 e2)    =
    S.union (freevars e1) (S.delete x $ freevars e2)

freevars (Var x)              = S.singleton x

freevars (LetRec (Fundef (x,_) yts e1) e2) = 
    let zs = (freevars e1) S.\\ (S.fromList $ map fst yts)
    in (S.union zs (freevars e2)) S.\\ S.singleton x

freevars (App x xs)           = S.fromList (x:xs)
freevars (Tuple xs)           = S.fromList xs
freevars (LetTuple xs y e)    = 
    S.insert y (freevars e S.\\ S.fromList (map fst xs))

freevars (Get x y)            = S.fromList [x,y]
freevars (Put x y z)          = S.fromList [x,y,z]
freevars (ExtArray _)         = S.empty
freevars (ExtFunApp _ xs)     = S.fromList xs




--------------------------------------------------------------------------------
-- k normalization

-- Need to also account for the typing env...


type Env = M.Map IdS T.Type  

newtype Mon a = Mon { getMon :: H.HMonad Env ExtEnv Int a }


instance Functor Mon where
  fmap f = Mon . fmap f . getMon

instance Applicative Mon where
  pure a    = Mon $ pure a
  af <*> av = Mon $ getMon af <*> getMon av

instance Monad Mon where
  return    = pure
  ma >>= k  = Mon $ getMon ma >>= (getMon . k)

instance MonadPlus Mon where
  mzero = Mon mzero
  ma `mplus` mb = Mon $ getMon ma `mplus` getMon mb


runMon :: Mon a -> Either String a
runMon ma = fmap fst $ H.runHMonad M.empty extEnv 1 (getMon ma) 


gentmp :: T.Type -> Mon IdS
gentmp ty = maybe (error "gentmp - TyVar") sk $ idOfType ty
  where
    sk ss = Mon $ H.puts (\i -> ('T':ss ++ show i,i+1))

localBinding :: (IdS,T.Type) -> Mon a -> Mon a
localBinding (v,t) ma = Mon $ H.local1 (M.insert v t) $ getMon ma

localBindings :: [(IdS,T.Type)] -> Mon a -> Mon a
localBindings vts ma = 
    Mon $ H.local1 (\env -> foldr fn env vts) $ getMon ma
  where
    fn (v,t) m = M.insert v t m

showBindings :: Mon String
showBindings = Mon $ H.asks1 show

findType :: IdS -> Mon T.Type
findType x = mfind >>= maybe mzero return 
  where
    mfind = Mon $ H.asks1 (M.lookup x)

findExtType :: IdS -> Mon T.Type
findExtType x = mfind >>= maybe mzero return
  where
    mfind = Mon $ H.asks2 (lookupExtEnv x)

-- | Quite complex behavior - has to check that the variable is
-- not bound in the local env first 
--
findExtUnique :: IdS -> Mon T.Type 
findExtUnique x = mfind1 >>= maybe sk (const mzero)
  where
    sk     = mfind2 >>= maybe mzero return 
    mfind1 = Mon $ H.asks1 (M.lookup x)
    mfind2 = Mon $ H.asks2 (lookupExtEnv x)


fatal :: String -> Mon a
fatal = Mon . H.fatal

failure :: String -> Mon a
failure = Mon . H.failure


infixl 1 `normalizeToLet`
normalizeToLet :: S.Expr
               -> (IdS -> T.Type -> Mon (Expr, T.Type)) 
               -> Mon (Expr, T.Type)
normalizeToLet e0 k = kn e0 >>= \a -> case a of
    (Var x, t) -> k x t
    (e,     t) -> do { x       <- gentmp t
                     ; (e',t') <- k x t
                     ; return (Let (x,t) e e', t')
                     }


infixl 1 `normalizeToLets`

normalizeToLets :: [S.Expr]
                -> ([IdS] -> [T.Type] -> Mon (Expr, T.Type))
                -> Mon (Expr, T.Type)
normalizeToLets es0 k = go [] [] es0
  where
    go vs ts []     = k vs ts
    go vs ts (e:es) = normalizeToLet e $ \v t -> go (vs++[v]) (ts++[t]) es

-- | Use rather than return after a normalizeToLet to indicate 
-- that the answer is inside let bindings.
--
interior :: (Expr, T.Type) -> Mon (Expr, T.Type)
interior = return




knormal :: S.Expr -> Either String Expr
knormal expr = fmap fst $ runMon $  kn expr

-- Do not use mplus in kn...

kn :: S.Expr -> Mon (Expr, T.Type)
kn S.Unit                   = return (Unit, T.Unit)

kn (S.Bool b)               = let val = if b then 1 else 0 
                              in return (Int val, T.Int)

kn (S.Int i)                = return (Int i, T.Int) 

kn (S.Float d)              = return (Float d, T.Float)


kn (S.Not e)                = 
    let s_simp = S.If e (S.Bool True) (S.Bool False) in kn s_simp


kn (S.Neg e)                = 
    e `normalizeToLet` \v t -> interior (Neg v, t)


kn (S.Add e1 e2)            = 
    e1 `normalizeToLet` \v1 _ -> 
    e2 `normalizeToLet` \v2 _ ->
    interior (Add v1 v2, T.Int)



kn (S.Sub e1 e2)            = 
    e1 `normalizeToLet` \v1 _ -> 
    e2 `normalizeToLet` \v2 _ ->
    interior (Sub v1 v2, T.Int)

    
kn (S.FNeg e)               = 
    e `normalizeToLet` \v t -> 
    interior (FNeg v, t)


kn (S.FAdd e1 e2)            = 
    e1 `normalizeToLet` \v1 _ -> 
    e2 `normalizeToLet` \v2 _ ->
    interior (FAdd v1 v2, T.Float)
  
kn (S.FSub e1 e2)            = 
    e1 `normalizeToLet` \v1 _ -> 
    e2 `normalizeToLet` \v2 _ ->
    interior (FSub v1 v2, T.Float)

kn (S.FMul e1 e2)            = 
    e1 `normalizeToLet` \v1 _ -> 
    e2 `normalizeToLet` \v2 _ ->
    interior (FMul v1 v2, T.Float)
    
kn (S.FDiv e1 e2)            = 
    e1 `normalizeToLet` \v1 _ -> 
    e2 `normalizeToLet` \v2 _ ->
    interior (FDiv v1 v2, T.Float)

kn cmp@(S.Eq {})             = 
    let s_simp = S.If cmp (S.Bool True) (S.Bool False) in kn s_simp

kn cmp@(S.LE {})             = 
    let s_simp = S.If cmp (S.Bool True) (S.Bool False) in kn s_simp

  
kn (S.If (S.Not e1) e2 e3)   = 
    let s_simp = S.If e1 e3 e2 in kn s_simp

kn (S.If (S.Eq e1 e2) e3 e4) = 
    e1 `normalizeToLet` \x _ -> 
    e2 `normalizeToLet` \y _ -> 
    kn e3 >>= \(ea, t1)    ->
    kn e4 >>= \(eb, _)     ->
    interior (IfEq x y ea eb, t1)

kn (S.If (S.LE e1 e2) e3 e4) = 
    e1 `normalizeToLet` \x _ -> 
    e2 `normalizeToLet` \y _ -> 
    kn e3 >>= \(ea, t1)    ->
    kn e4 >>= \(eb, _)     ->
    interior (IfLE x y ea eb, t1)

kn (S.If e1 e2 e3)           = 
    let s_simp = S.If (S.Eq e1 (S.Bool False)) e3 e2 in kn s_simp

kn (S.Let (v,t) e1 e2)      = do 
    (ea, _ ) <- kn e1
    (eb, t2) <- localBinding (v,t) $ kn e2
    return (Let (v,t) ea eb, t2)


kn (S.Var x)                =
    bound_var <<|> ext_var <<|> not_found
  where
    bound_var = findType x >>= \t -> return (Var x, t)
    ext_var   = findExtType x >>= \t -> case t of
                   T.Array {} -> return (ExtArray x , t)
                   _          -> fatal $ "external variable not an array " ++ x

    not_found = fatal $ "kn - Var - unbound variable " ++ x



kn (S.LetRec (S.Fundef (x,t) yts e1) e2) = 
    localBinding (x,t) $ do
       (eb,t2) <- kn e2
       (ea,_)  <- localBindings yts $ kn e1
       return (LetRec (Fundef (x,t) yts ea) eb, t2)

-- f should only be in the ext_env if it is a foreign function.

kn (S.App e1 e2s)           = case e1 of
    S.Var f -> foreignFun f <<|> localFun f 
    _       -> reduction
  where
    foreignFun x = findExtUnique x >>= \ext_t -> case ext_t of
        T.Fun _ ty -> e2s `normalizeToLets` \xs _ ->
                      interior (ExtFunApp x xs, ty)
        _          -> fatal $ "kn - apply an external non-function"

    localFun x   = findType x >>= \ext_t -> case ext_t of
        T.Fun _ ty -> e2s `normalizeToLets` \xs _ ->
                      interior (App x xs, ty)
        _          -> showBindings >>= \ss ->
                      fatal $ "kn - apply to internal non-function " ++ show e1
                              ++ "\n" ++ ss

    reduction    = e1 `normalizeToLet` \x ty -> case ty of
        T.Fun _ t -> e2s `normalizeToLets` \xs _ -> 
                     interior (App x xs, t)
        _         -> fatal $ "kn - application of a non-function after reduction"
        

  

{-

kn (S.Tuple elts)           = bind [] [] elts
  where
    bind xs ts []     = return (Tuple xs, T.Tuple ts)
    bind xs ts (e:es) = kn e `normalizeToLet` \x t -> 
                        bind (xs++[x]) (ts++[t]) es
  

kn (S.LetTuple xts e1 e2)   = 
    kn e1 `normalizeToLet` \x _ ->
    addlocals xts $
        kn e2 >>= \(e2',t2) ->
        return (LetTuple xts x e2', t2)
        
kn (S.Array e1 e2)          = 
    kn e1 `normalizeToLet` \x _  -> 
    kn e2 `normalizeToLet` \y t2 ->
    let l = case t2 of { T.Float -> "create_float_array"; _ -> "create_array" }
    in return (ExtFunApp l [x,y], T.Array t2)

kn (S.Get e1 e2)            = kn e1 >>= \ans -> case ans of
    (_, T.Array ty) -> kn e1 `normalizeToLet` \x _ -> 
                       kn e2 `normalizeToLet` \y _ -> 
                       return (Get x y, ty)
    _               -> error "kn - ill-formed Get expression."
-}
kn (S.Put e1 e2 e3)         = 
    e1 `normalizeToLet` \v1 _ -> 
    e2 `normalizeToLet` \v2 _ -> 
    e3 `normalizeToLet` \v3 _ -> 
    interior (Put v1 v2 v3, T.Unit)

