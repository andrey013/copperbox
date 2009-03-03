{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# OPTIONS -Wall #-}

-- |
-- Module: HMinCaml.Alpha
-- License: as per original MinCaml
--
-- Maintainer: Stephen Tetley <stephen.tetley@gmail.com>
-- Stability: unstable
-- Portability: ghc
--
-- Alpha conversion
--

module HMinCaml.Alpha where

import HMinCaml.CompilerMonad
import HMinCaml.Id
import qualified HMinCaml.M as M
import HMinCaml.KNormal

import Control.Applicative
import Control.Monad.State

type Env = M.M Id Id

find :: Id -> Env -> Id
find x env = maybe x id (M.find x env) 



g :: (Applicative m, MonadState Int m) => Env -> Expr -> m Expr
g _   Unit                = return $ Unit
g _   (Int i)             = return $ Int i
g _   (Float d)           = return $ Float d
g env (Neg x)             = return $ Neg (find x env)
g env (Add x y)           = return $ Add (find x env) (find y env)
g env (Sub x y)           = return $ Sub (find x env) (find y env)
g env (FNeg x)            = return $ FNeg (find x env)
g env (FAdd x y)          = return $ FAdd (find x env) (find y env)
g env (FSub x y)          = return $ FSub (find x env) (find y env)
g env (FMul x y)          = return $ FMul (find x env) (find y env)
g env (FDiv x y)          = return $ FDiv (find x env) (find y env)
g env (IfEq x y e1 e2)    = IfEq <$> pure (find x env) <*> pure (find y env) 
                                 <*> (g env e1)        <*> (g env e2)
                                 
g env (IfLE x y e1 e2)    = IfLE <$> pure (find x env) <*> pure (find y env) 
                                 <*> (g env e1)        <*> (g env e2)
                                 
g env (Let (x, t) e1 e2)  = do x'   <- genid x
                               e1'   <- g env e1
                               e2'   <- g (M.add x x' env) e2
                               return $ Let (x', t) e1' e2'
                              
g env (Var x)             = return $ Var (find x env)
g env (LetRec (Fundef (x,t) yts e1) e2) = do
      x'          <- genid x
      let env1    = M.add x x' env
      let ys      = map fst yts
      ys'         <- mapM genid ys
      let env'    = M.addList2 ys ys' env1
      let name_   = (find x env1, t)
      let args_   = map (\(y, ty) -> (find y env', ty)) yts
      body_       <- g env' e1
      e2'         <- g env1 e2
      return $ LetRec (Fundef name_ args_ body_) e2'

g env (App x ys)          = return $ App (find x env) (map (\y -> find y env) ys)
g env (Tuple xs)          = return $ Tuple (map (\x -> find x env) xs)
g env (LetTuple xts y e)  = let xs = map fst xts in do
      xs'         <- mapM genid xs
      let env'    = M.addList2 xs xs' env
      let xts'    = map (\(x,ty) -> (find x env',ty)) xts
      e'          <- g env' e 
      return $ LetTuple xts' (find y env) e'
      
g env (Get x y)           = return $ Get (find x env) (find y env)
g env (Put x y z)         = return $ Put (find x env) (find y env) (find z env)
g _   (ExtArray x)        = return $ ExtArray x
g env (ExtFunApp x ys)    = return $ ExtFunApp x (map (find `flip` env) ys)
         


alpha :: Expr -> Expr
alpha e = eval (g M.empty e) 0