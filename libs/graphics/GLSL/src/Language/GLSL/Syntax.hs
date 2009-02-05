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

data SlTranslUnit = SlTranslUnit [SlGblDecl]

data SlGblDecl = SlGblFunDef SlFunDef
               | SlGblDecl SlDecl 
  deriving (Eq,Show,Typeable,Data)

data SlFunDef = SlFunDef SlFunProto 
                         SlStmt         -- Compound statement
  deriving (Eq,Show,Typeable,Data)

  
data SlConst = SlIntConst Integer
             | SlFloatConst String        -- language-c represents as strings too
             | SlBoolConst Bool 
  deriving (Eq,Show,Typeable,Data)


data SlDecl = SlDecl
  deriving (Eq,Show,Typeable,Data)
  
data SlDeclr = SlDeclr
  deriving (Eq,Show,Typeable,Data)
  
data SlStruct = Struct (Maybe Ident)
                       [SlDecl]
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

data SlExpr = ConstantExpr SlConst
            | ContructorExpr
            
            | VarExpr Ident
            | AssignExpr AssignOp 
                         SlExpr 
                         SlExpr
            | UnaryExpr  UnaryOp
                         SlExpr
            | BinaryExpr BinaryOp
                         SlExpr
                         SlExpr
            | CommaExpr [SlExpr]      -- seqeuence of expressions
            | TernaryExpr SlExpr      -- (? :)
                          SlExpr
                          SlExpr            
  deriving (Eq,Show,Typeable,Data)


data SlFunProto = SlFunProto
  deriving (Eq,Show,Typeable,Data)
  
  
-- p73  
data SlParamQual = In | Out | Inout
  deriving (Eq,Show,Typeable,Data)

data SlTypeQual = Const
                | Attribute
                | Varying [SlVaryingQual]
                | Uniform
  deriving (Eq,Show,Typeable,Data)
  
data SlVaryingQual = Centroid
                   | Invariant
  deriving (Eq,Show,Typeable,Data)
  
  
data SlTypeSpec = SlVoid
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
                | StructType SlStruct
              -- TYPE_NAME
  deriving (Eq,Show,Typeable,Data)
 

data SlStmt = CompoundStmt [SlStmt]
            | DeclStmt SlDecl
            | ExprStmt (Maybe SlExpr)
            | IfStmt   SlExpr
                       SlStmt
                       (Maybe SlStmt)
            | For      SlExpr               -- init
                       SlExpr               -- condition
                       (Maybe SlExpr)       -- loop_expr 
                       SlStmt
            | While    SlExpr
                       SlStmt
            | DoWhile  SlStmt
                       SlExpr                    
            | Continue
            | Break
            | Return (Maybe SlExpr)
            | Discard
  deriving (Eq,Show,Typeable,Data)      
  


    