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
import HMinCaml.Type

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
  
fv e = error "SparcAsm fv todo"  
