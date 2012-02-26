{-# LANGUAGE DeriveDataTypeable         #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Language.GLSL.Syntax
-- Copyright   :  (c) Stephen Tetley 2012
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Datatypes to describe GLSL (the OpenGL Shading Language).
-- 
--------------------------------------------------------------------------------



module Language.GLSL.Syntax 
  (
    Ident
  , FieldSelector
  , FieldSelection
  
  , TranslUnit(..)
  , GblDecl(..)
  , FunDef(..)
  , Constant(..)
  , Decl(..)
  , Declrs(..)
  , DeclrElement(..)
  , Struct(..)
  , StructDeclr(..)
  , StructDeclrElement(..)
  , UnaryOp(..)
  , BinaryOp(..)
  , AssignOp(..)
  , Expr(..)
  , FunProto(..)
  , ParamDecl(..)
  , ParamDeclr(..)
  , ParamQual(..)
  , FullType
  , TypeQual(..)
  , VaryingQual(..)
  , TypeSpec(..)
  , ScalarTypeSpec(..)
  , Stmt(..)

  )  where

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
          | InitDeclr Declrs         -- more concrete syntax than abstract
  deriving (Eq,Show,Typeable,Data)


-- more concrete syntax than abstract (should simplify to one declr / one type)  
data Declrs = 
        Declr          FullType 
                       [DeclrElement]
      | InvariantDeclr Ident 
                       [DeclrElement]   -- too permissive but seemingly legal
  deriving (Eq,Show,Typeable,Data)


data DeclrElement = 
        ScalarDeclr Ident                   
                    (Maybe Expr)     -- initializer
      | ArrayDeclr  Ident
                    (Maybe Expr)     -- array size expr
                    (Maybe Expr)                         
  deriving (Eq,Show,Typeable,Data)
  
data Struct = Struct (Maybe Ident)
                     [StructDeclr]
  deriving (Eq,Show,Typeable,Data)

data StructDeclr = 
        StructDeclr TypeSpec
                    [StructDeclrElement]
  deriving (Eq,Show,Typeable,Data)

  
data StructDeclrElement = 
        StructScalarDeclr Ident                   
      | StructArrayDeclr  Ident
                          Expr     -- array size expr                        
  deriving (Eq,Show,Typeable,Data)

-- p70
data UnaryOp = PreIncOp
             | PreDecOp
             | PostIncOp
             | PostDecOp
             | PlusOp   
             | MinusOp  
             | LNotOp   
             | NotOp
  deriving (Eq,Show,Typeable,Data)

data BinaryOp = MulOp             
              | DivOp             
              | RemainderOp       
              | AddOp             
              | SubOp             
              | ShiftLOp
              | ShiftROp
              | LtOp    
              | GtOp    
              | LteOp   
              | GteOp   
              | EqOp    
              | NeqOp   
              | AndOp   
              | XorOp   
              | OrOp    
              | LandOp  
              | LxorOp  
              | LorOp
  deriving (Eq,Show,Typeable,Data)
  
data AssignOp = AssignOp
              | MulAssign
              | DivAssign
              | ModAssign
              | AddAssign
              | SubAssign
              | LShiftAssign
              | RShiftAssign
              | AndAssign   
              | XorAssign   
              | OrAssign
  deriving (Eq,Show,Typeable,Data)

data Expr = ConstantExpr Constant
          | ContructorExpr
          
          | VarExpr    Ident
          | NewVar     FullType           -- this is a new variable introduced
                       Ident              -- in a condition (ideally it shouldn't
                       Expr               -- be in the /abstract/ syntax tree).           
          | AssignExpr AssignOp 
                       Expr 
                       Expr
          | UnaryExpr  UnaryOp
                       Expr
          | BinaryExpr BinaryOp
                       Expr
                       Expr
          | ArrayAccessExpr Expr
                            Expr
          | FieldAccessExpr Expr
                            FieldSelection 
          | MethodAccessExpr Expr 
                             Expr                                                   
          | FunCallExpr Ident
                        [Expr]    -- arguments are expressions
          | CommaExpr   [Expr]      -- seqeuence of expressions
          
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

data ParamDeclr = 
        ParamScalarDeclr TypeSpec
                         Ident
      | ParamArrayDeclr  TypeSpec
                         Ident
                         Expr  
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
  

data TypeSpec = ScalarType ScalarTypeSpec 
              | ArrayType  ScalarTypeSpec
                           Expr                 -- constant_expr
  deriving (Eq,Show,Typeable,Data)
  
  
  
data ScalarTypeSpec = SlVoid
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
                    | TypeName String
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
  


    