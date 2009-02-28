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
        Ans     SparcExp
      | Let     (Id,Type)   SparcExp  SparcT
      | Forget  Id          SparcT              {- virtual instruction -}
  deriving (Eq,Show)
  
data SparcExp =
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
      | IfEq    Id Id_or_Imm SparcExp SparcExp
      | IfLE    Id Id_or_Imm SparcExp SparcExp
      | IfGE    Id Id_or_Imm SparcExp SparcExp
      | IfFEq   Id Id SparcExp SparcExp
      | IfFLE   Id Id SparcExp SparcExp
      {- closure address, integer arguments, and float arguments -}
      | CallCls Id [Id] [Id]
      | CallDir Label [Id] [Id]
      | Save    Id Id
      | Restore Id
  deriving (Eq,Show)
  
  
data SparcFundef = SparcFundef  
      { fun_name  :: Label
      , args      :: [Id]
      , fargs     :: [Id]
      , body      :: SparcT
      , ret       :: Type 
      }
  deriving (Eq,Show)
  
  

data Prog = Prog [(Label, Float)] [SparcFundef] SparcT
  deriving (Eq,Show)
  
  
{-
val fletd : Id.t * exp * t -> t (* shorthand of Let for float *)
val seq : exp * t -> t (* shorthand of Let for unit *)

val regs : Id.t array
val fregs : Id.t array
val allregs : Id.t list
val allfregs : Id.t list
val reg_cl : Id.t
val reg_sw : Id.t
val reg_fsw : Id.t
val reg_ra : Id.t
val reg_hp : Id.t
val reg_sp : Id.t
val is_reg : Id.t -> bool
val co_freg : Id.t -> Id.t

val fv : t -> Id.t list
val concat : t -> Id.t * Type.t -> t -> t

val align : int -> int

-}
