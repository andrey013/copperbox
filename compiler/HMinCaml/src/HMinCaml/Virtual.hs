{-# OPTIONS -Wall #-}

-- |
-- Module: HMinCaml.Virtual
-- License: as per original MinCaml
--
-- Maintainer: Stephen Tetley <stephen.tetley@gmail.com>
-- Stability: unstable
-- Portability: ghc
--
-- Translate to Sparc assembly with infinite registers
--

module HMinCaml.Virtual where

import qualified HMinCaml.Closure as C
import HMinCaml.Id
import qualified HMinCaml.M as M
import HMinCaml.SparcAsm
import HMinCaml.Type

import Data.List ( foldl' )

classify xts ini addf addi = foldl' fn ini xts where
    fn acc (_,TUnit)    = acc
    fn acc (x,TFloat)   = addf acc x
    fn acc (x,t)        = addi acc x t

separate xts = classify xts ([], []) ff fi where
    ff (int,float) x    = (int, float ++ [x])
    fi (int,float) x _  = (int ++ [x], float)

expand xts ini addf addi = classify xts ini ff fi where
    ff (offset,acc) x     = let offset' = align offset
                            in (offset' + 8, addf x offset' acc)
    fi (offset, acc) x t  = (offset + 4, addi (x,t) offset acc)

g :: M.M Id Type -> C.Expr -> SparcT
g env C.Unit              = Ans Nop
g env (C.Int i)           = Ans (Set i)
{-
g env (C.Float d)         = 
      let l =
  try
    (* すでに定数テーブルにあったら再利用 *)
    let (l, _) = List.find (fun (_, d') -> d = d') !data in
    l
  with Not_found ->
    let l = Id.L(Id.genid "l") in
    data := (l, d) :: !data;
    l in
      let x = Id.genid "l" in
      Let((x, Type.Int), SetL(l), Ans(LdDF(x, C(0))))
-}
g env (C.Neg x)             = Ans (Neg x)
g env (C.Add x y)           = Ans (Add x (V y))
g env (C.Sub x y)           = Ans (Sub x (V y))
g env (C.FNeg x)            = Ans (FNegD x)
g env (C.FAdd x y)          = Ans (FAddD x y)
g env (C.FSub x y)          = Ans (FSubD x y)
g env (C.FMul x y)          = Ans (FMulD x y)
g env (C.FDiv x y)          = Ans (FDivD x y)
g env (C.IfEq x y e1 e2)    = case M.find x env of
    Just TBool    -> Ans (IfEq x (V y) (g env e1) (g env e2))
    Just TInt     -> Ans (IfEq x (V y) (g env e1) (g env e2)) 
    Just TFloat   -> Ans (IfFEq x y (g env e1) (g env e2))
    _             -> error "equality supported only for bool, int, and float"

g env (C.IfLE x y e1 e2)    = case M.find x env of
    Just TBool    -> Ans (IfLE x (V y) (g env e1) (g env e2))
    Just TInt     -> Ans (IfLE x (V y) (g env e1) (g env e2))
    Just TFloat   -> Ans (IfFLE x y (g env e1) (g env e2))
    _             -> error "inequality supported only for bool, int and float"

g env (C.Let (x,t1) e1 e2)  =
    let e1' = g env e1
        e2' = g (M.add x t1 env) e2
    in sparcConcat e1' (x, t1) e2'

g env (C.Var x)             = case M.find x env of
    Just TUnit    -> Ans Nop
    Just TFloat   -> Ans (FMovD x)
    _             -> Ans (Mov x)


virtual :: C.Prog -> Prog
virtual _ = error "virtual undefined"

         