{-# OPTIONS -Wall #-}

-- |
-- Module: HMinCaml.Closure
-- License: as per original MinCaml
--
-- Maintainer: Stephen Tetley <stephen.tetley@gmail.com>
-- Stability: unstable
-- Portability: ghc
--
-- Closure conversion
--

module HMinCaml.Closure where

import HMinCaml.Id
import qualified HMinCaml.KNormal as K
import qualified HMinCaml.M as M
import qualified HMinCaml.S as S
import HMinCaml.Type


data Closure = Closure { entry :: Label, actual_fv :: [Id] }
  deriving (Eq,Show)
  

data Expr = Unit
          | Int       Int
          | Float     Float
          | Neg       Id
          | Add       Id  Id
          | Sub       Id  Id
          | FNeg      Id
          | FAdd      Id  Id
          | FSub      Id  Id
          | FMul      Id  Id
          | FDiv      Id  Id
          | IfEq      Id  Id  Expr  Expr
          | IfLE      Id  Id  Expr  Expr
          | Let       (Id,Type) Expr  Expr
          | Var       Id
          | MakeCls   (Id,Type) Closure Expr
          | AppCls    Id [Id]
          | AppDir    Label [Id]
          | Tuple     [Id]
          | LetTuple  [(Id,Type)] Id  Expr
          | Get       Id  Id
          | Put       Id  Id  Id
          | ExtArray  Label
  deriving (Eq,Show)
    
data Fundef = Fundef 
      { name        :: (Label, Type)
      , args        :: [(Id, Type)]
      , formal_fv   :: [(Id, Type)]
      , body        :: Expr 
      }
  deriving (Eq,Show)
  
data Prog = Prog [Fundef] Expr
  deriving (Eq,Show)


fv :: Expr -> S.S Id
fv Unit                             = S.empty
fv (Int _)                          = S.empty
fv (Float _)                        = S.empty
fv (ExtArray _)                     = S.empty
fv (Neg x)                          = S.singleton x
fv (FNeg x)                         = S.singleton x
fv (Add x y)                        = S.ofList [x, y]
fv (Sub x y)                        = S.ofList [x, y]
fv (FAdd x y)                       = S.ofList [x, y]
fv (FSub x y)                       = S.ofList [x, y]
fv (FMul x y)                       = S.ofList [x, y]
fv (FDiv x y)                       = S.ofList [x, y]
fv (Get x y)                        = S.ofList [x, y]
fv (IfEq x y e1 e2)                 = S.add x (S.add y (S.union (fv e1) (fv e2)))
fv (IfLE x y e1 e2)                 = S.add x (S.add y (S.union (fv e1) (fv e2)))
fv (Let (x,_) e1 e2)                = S.union (fv e1) (S.remove x (fv e2))
fv (Var x)                          = S.singleton x
fv (MakeCls (x,_) (Closure _ ys) e) = S.remove x (S.union (S.ofList ys) (fv e))
fv (AppCls x ys)                    = S.ofList (x : ys)
fv (AppDir _  xs)                   = S.ofList xs 
fv (Tuple xs)                       = S.ofList xs
fv (LetTuple xts y e)               = S.add y (S.diff (fv e) 
                                                      (S.ofList (map fst xts)))
fv (Put x y z)                      = S.ofList [x, y, z]


g env known a = step a where
    step K.Unit               = Unit
    step (K.Int i)            = Int i
    step (K.Float d)          = Float d
    step (K.Neg x)            = Neg x
    step (K.Add x y)          = Add x y
    step (K.Sub x y)          = Sub x y
    step (K.FNeg x)           = FNeg x
    step (K.FAdd x y)         = FAdd x y
    step (K.FSub x y)         = FSub x y
    step (K.FMul x y)         = FMul x y
    step (K.FDiv x y)         = FDiv x y
    step (K.IfEq x y e1 e2)   = IfEq x y (g env known e1) (g env known e2)
    step (K.IfLE x y e1 e2)   = IfLE x y (g env known e1) (g env known e2)
    step (K.Let (x,t) e1 e2)  = Let (x,t) (g env known e1) 
                                          (g (M.add x t env) known e2)
    step (K.Var x)            = Var x
   
    step (K.LetRec _ _)       = error "todo"

    step (K.App x ys) 
        |  S.mem x known      = AppDir (L x) ys
        | otherwise           = AppCls x ys
    step (K.Tuple xs)         = Tuple xs
    step (K.LetTuple xts y e) = LetTuple xts  y (g (M.addList xts env) known e)
    step (K.Get x y)          = Get x y
    step (K.Put x y z)        = Put x y z
    step (K.ExtArray x)       = ExtArray (L x)
    step (K.ExtFunApp x ys)   = AppDir (L $ "min_caml_" ++ x) ys
      