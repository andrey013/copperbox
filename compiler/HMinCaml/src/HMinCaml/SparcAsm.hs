{-# LANGUAGE FlexibleContexts           #-}
{-# OPTIONS -Wall #-}

-- |
-- Module: HMinCaml.SparcAsm
-- License: as per original MinCaml
--
-- Maintainer: Stephen Tetley <stephen.tetley@gmail.com>
-- Stability: unstable
-- Portability: ghc
--
-- Sparc asm
--

module HMinCaml.SparcAsm where

import HMinCaml.Id
import qualified HMinCaml.M as M
import qualified HMinCaml.S as S
import HMinCaml.Type

import Control.Applicative
import Control.Monad.State
import Data.Array.IArray


data Id_or_Imm = V Id | C Int
  deriving (Eq,Show)

data SparcT =
        Ans     Expr
      | Let     (Id,Type)   Expr  SparcT
      | Forget  Id          SparcT              {- virtual instruction -}
  deriving (Eq,Show)
  
data Expr =
        Nop
      | Set     Int
      | SetL    Id
      | Mov     Id
      | Neg     Id
      | Add     Id Id_or_Imm
      | Sub     Id Id_or_Imm
      | SLL     Id Id_or_Imm
      | Ld      Id Id_or_Imm
      | St      Id Id Id_or_Imm
      | FMovD   Id
      | FNegD   Id
      | FAddD   Id Id
      | FSubD   Id Id
      | FMulD   Id Id
      | FDivD   Id Id
      | LdDF    Id Id_or_Imm
      | StDF    Id Id Id_or_Imm
      | Comment String
      {- virtual instructions -}
      | IfEq    Id Id_or_Imm  SparcT SparcT
      | IfLE    Id Id_or_Imm  SparcT SparcT
      | IfGE    Id Id_or_Imm  SparcT SparcT
      | IfFEq   Id Id         SparcT SparcT
      | IfFLE   Id Id         SparcT SparcT
      {- closure address, integer arguments, and float arguments -}
      | CallCls Id [Id] [Id]
      | CallDir Label [Id] [Id]
      | Save    Id Id
      | Restore Id
  deriving (Eq,Show)
  
  
data Fundef = Fundef  
      { fun_name  :: Label
      , args      :: [Id]
      , fargs     :: [Id]
      , body      :: SparcT
      , ret       :: Type 
      }
  deriving (Eq,Show)
  
  

data Prog = Prog [(Label, Float)] [Fundef] SparcT
  deriving (Eq,Show)

fletd :: Id -> Expr -> SparcT -> SparcT
fletd x e1 e2 = Let (x, TFloat) e1 e2

-- ulet aka seq 
ulet :: (MonadState Int m, Applicative m) 
     => Expr -> SparcT -> m SparcT
ulet e1 e2  = (\ident -> Let (ident, TUnit) e1 e2) <$> gentmp TUnit

regs :: Array Int Id
regs = listArray (0,length xs -1) xs where
    xs = ["%i2", "%i3", "%i4", "%i5",
          "%l0", "%l1", "%l2", "%l3", "%l4", "%l5", "%l6", "%l7",
          "%o0", "%o1", "%o2", "%o3", "%o4", "%o5" ]
          
fregs :: Array Int Id
fregs = listArray (0,length xs -1) xs where
    xs = map (\i -> '%':'f': show (i * 2) ) [0..15::Int]

allregs :: [Id]                    
allregs = elems regs

allfregs :: [Id]
allfregs = elems fregs

-- Closure address
reg_cl :: Id
reg_cl = let (_,i) = bounds regs in regs!i

reg_sw :: Id
reg_sw = let (_,i) = bounds regs in regs!(i-1)

reg_swf :: Id
reg_swf = let (_,i) = bounds fregs in fregs!i

-- stack pointer
reg_sp :: Id
reg_sp = "%i0" 

-- heap pointer 
reg_hp :: Id
reg_hp = "%i1" 

--  return address
reg_ra :: Id 
reg_ra = "%o7" 

isReg :: Id -> Bool
isReg ('%':_)  = True
isReg _        = False

co_freg_table :: M.M Id Id
co_freg_table = M.addList xs M.empty where
  xs = map (\i -> ('%':'f':show (i*2), '%':'f':show (i*2+1))) [0..15::Int]  

-- "companion" freg
coFreg :: Id -> Id
coFreg freg = M.find' freg co_freg_table

removeAndUniq :: S.S Id -> [Id] -> [Id]
removeAndUniq set ls = step ls where
    step []     = []
    step (y:ys) | S.mem y set   = removeAndUniq set ys
                | otherwise     = removeAndUniq (S.add y set) ys

fv_idOrImm :: Id_or_Imm -> [Id]
fv_idOrImm (V x) = [x] 
fv_idOrImm  _    = []

fv_exp :: [Id] -> Expr -> [Id]
fv_exp cont Nop               = cont
fv_exp cont (Set _)           = cont
fv_exp cont (SetL _)          = cont 
fv_exp cont (Comment _)       = cont 
fv_exp cont (Restore _)       = cont
fv_exp cont (Mov x)           = x:cont
fv_exp cont (Neg x)           = x:cont
fv_exp cont (FMovD x)         = x:cont
fv_exp cont (FNegD x)         = x:cont
fv_exp cont (Save x _)        = x:cont
fv_exp cont (Add x y')        = (x:fv_idOrImm y') ++ cont
fv_exp cont (Sub x y')        = (x:fv_idOrImm y') ++ cont
fv_exp cont (SLL x y')        = (x:fv_idOrImm y') ++ cont
fv_exp cont (Ld x y')         = (x:fv_idOrImm y') ++ cont
fv_exp cont (LdDF x y')       = (x:fv_idOrImm y') ++ cont
fv_exp cont (St x y z')       = x:y: fv_idOrImm z' ++ cont
fv_exp cont (StDF x y z')     = x:y: fv_idOrImm z' ++ cont
fv_exp cont (FAddD x y)       = x:y:cont
fv_exp cont (FSubD x y)       = x:y:cont
fv_exp cont (FMulD x y)       = x:y:cont
fv_exp cont (FDivD x y)       = x:y:cont
fv_exp cont (IfEq x y' e1 e2) = x: fv_idOrImm y' ++ removeAndUniq S.empty (fv' cont e1 ++ fv' cont e2)
fv_exp cont (IfLE x y' e1 e2) = x: fv_idOrImm y' ++ removeAndUniq S.empty (fv' cont e1 ++ fv' cont e2)
fv_exp cont (IfGE x y' e1 e2) = x: fv_idOrImm y' ++ removeAndUniq S.empty (fv' cont e1 ++ fv' cont e2)
fv_exp cont (IfFEq x y e1 e2) = x:y: removeAndUniq S.empty (fv' cont e1 ++ fv' cont e2)
fv_exp cont (IfFLE x y e1 e2) = x:y: removeAndUniq S.empty (fv' cont e1 ++ fv' cont e2)
fv_exp cont (CallCls x ys zs) = x:ys ++ zs ++ cont
fv_exp cont (CallDir _ ys zs) = ys ++ zs ++ cont

fv' :: [Id] -> SparcT -> [Id]
fv' cont (Ans expr)           = fv_exp cont expr
fv' cont (Let (x,_) expr e)   = 
      let cont' = removeAndUniq (S.singleton x) (fv' cont e) in
      fv_exp cont' expr
fv' cont (Forget x e)         = removeAndUniq (S.singleton x) (fv' cont e)
  
  
fv :: SparcT -> [Id]
fv e = removeAndUniq S.empty (fv' [] e)   


sparcConcat :: SparcT -> (Id, Type) -> SparcT -> SparcT
sparcConcat (Ans expr)        xt e2 = Let xt expr e2
sparcConcat (Let yt expr e1') xt e2 = Let yt expr (sparcConcat e1' xt e2)
sparcConcat (Forget y e1')    xt e2 = Forget y (sparcConcat e1' xt e2)

align :: Int -> Int  
align i = if i `mod` 8 == 0 then i else i + 4



