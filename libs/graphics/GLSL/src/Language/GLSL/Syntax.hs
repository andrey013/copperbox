{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Language.GLSL.Syntax
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Datatypes to describe GLSL (the OpenGL Shading Language).
-- 
--------------------------------------------------------------------------------



module Language.GLSL.Syntax  where

import Data.Generics.Basics
import Data.Generics.Instances()

type Ident = String

-- the following sets are defined {r,g,b,a} {x,y,z,w} {s,t,p,q}
type FieldSelector = Char 
type FieldSelection = [FieldSelector]

data TranslUnit = TranslUnit [GblDecl]
  deriving (Eq,Show,Typeable,Data)
  
data GblDecl = GblFunDef FunDef
             | GblDecl Decl 
  deriving (Eq,Show,Typeable,Data)

data FunDef = FunDef FunProto 
                     Stmt         -- Compound statement
  deriving (Eq,Show,Typeable,Data)

  
data Constant = IntConst    Integer
              | FloatConst  String        -- language-c represents as strings too
              | BoolConst   Bool 
  deriving (Eq,Show,Typeable,Data)


data Decl = FunProtoDecl FunProto
          | InitialDeclrs [Declr]   -- not really correct might have specifiers not just declrs
  deriving (Eq,Show,Typeable,Data)
  
-- aka single_declaration  
data Declr = Declr FullType
                   (Maybe Ident)
                   (Maybe Expr)     -- array size expr
                   (Maybe Expr)     -- initializer
  deriving (Eq,Show,Typeable,Data)
  
data Struct = Struct (Maybe Ident)
                     [Decl]
  deriving (Eq,Show,Typeable,Data)

-- p70
data UnaryOp = PreIncOp           -- ++a
             | PreDecOp           -- --a
             | PostIncOp          -- a++
             | PostDecOp          -- a--
             | PlusOp             -- +
             | MinusOp            -- -
             | LNotOp             -- !
             | NotOp              -- ~
  deriving (Eq,Show,Typeable,Data)

data BinaryOp = MulOp             -- *
              | DivOp             -- /
              | RemainderOp       -- %
              | AddOp             -- +
              | SubOp             -- - 
              | ShiftLOp          -- <<
              | ShiftROp          -- >>
              | LtOp              -- <
              | GtOp              -- >
              | LteOp             -- <=
              | GteOp             -- >=
              | EqOp              -- ==
              | NeqOp             -- !=
              | AndOp             -- &
              | XorOp             -- ^
              | OrOp              -- |
              | LandOp            -- &&
              | LxorOp            -- ^^
              | LorOp             -- ||
  deriving (Eq,Show,Typeable,Data)
  
data AssignOp = AssignOp        -- =
              | MulAssign       -- *=
              | DivAssign       -- /=
              | ModAssign       -- %= (illegal)
              | AddAssign       -- +=
              | SubAssign       -- -=
              | LShiftAssign    -- <<=
              | RShiftAssign    -- >>=
              | AndAssign       -- &=
              | XorAssign       -- ^=
              | OrAssign        -- |=
  deriving (Eq,Show,Typeable,Data)

data Expr = ConstantExpr Constant
          | ContructorExpr
          
          | VarExpr Ident
          | AssignExpr AssignOp 
                       Expr 
                       Expr
          | UnaryExpr  UnaryOp
                       Expr
          | BinaryExpr BinaryOp
                       Expr
                       Expr
          | CommaExpr [Expr]      -- seqeuence of expressions
          | TernaryExpr Expr      -- (? :)
                        Expr
                        Expr            
  deriving (Eq,Show,Typeable,Data)


data FunProto = FunProto FullType 
                         Ident 
                         [ParamDecl]
  deriving (Eq,Show,Typeable,Data)

data ParamDecl = Declarator (Maybe TypeQual)
                            ParamQual
                            ParamDeclr
               | Specifier  (Maybe TypeQual)
                            ParamQual
                            TypeSpec
  deriving (Eq,Show,Typeable,Data)

data ParamDeclr = ParamDeclr TypeSpec
                             Ident
                             (Maybe Expr)  
  deriving (Eq,Show,Typeable,Data)
  
  
-- p73  
data ParamQual = In | Out | Inout
  deriving (Eq,Show,Typeable,Data)


type FullType = (Maybe TypeQual, TypeSpec) 

data TypeQual = Const
              | Attribute
              | Varying [VaryingQual]
              | Uniform
  deriving (Eq,Show,Typeable,Data)
  
data VaryingQual = Centroid
                 | Invariant
  deriving (Eq,Show,Typeable,Data)
  
  
data TypeSpec = SlVoid
              | SlFloat
              | SlInt
              | SlBool
              | Vec2
              | Vec3
              | Vec4
              | BVec2
              | BVec3
              | BVec4
              | IVec2
              | IVec3
              | IVec4
              | Mat2
              | Mat3
              | Mat4
              | Mat2x2
              | Mat2x3
              | Mat2x4
              | Mat3x2
              | Mat3x3
              | Mat3x4
              | Mat4x2
              | Mat4x3
              | Mat4x4
              | Sampler1D
              | Sampler2D
              | Sampler3D
              | SamplerCube
              | Sampler1DShadow
              | Sampler2DShadow
              | StructType Struct
              -- TYPE_NAME
  deriving (Eq,Show,Typeable,Data)
 

data Stmt = CompoundStmt [Stmt]
            | DeclStmt Decl
            | ExprStmt (Maybe Expr)
            | IfStmt   Expr
                       Stmt
                       (Maybe Stmt)
            | For      Expr               -- init
                       Expr               -- condition
                       (Maybe Expr)       -- loop_expr 
                       Stmt
            | While    Expr
                       Stmt
            | DoWhile  Stmt
                       Expr                    
            | Continue
            | Break
            | Return (Maybe Expr)
            | Discard
  deriving (Eq,Show,Typeable,Data)      
  


    