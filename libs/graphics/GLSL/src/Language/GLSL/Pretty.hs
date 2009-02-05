{-# OPTIONS -Wall #-}
{-# OPTIONS -fno-warn-orphans #-}



--------------------------------------------------------------------------------
-- |
-- Module      :  Language.GLSL.Pretty
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Pretty print ...
-- Note uses Daan Leijen's pretty printer and has /hides/ orphans.
-- 
--------------------------------------------------------------------------------



module Language.GLSL.Pretty where

import Language.GLSL.Syntax

import Text.PrettyPrint.Leijen


instance Pretty Constant where
  pretty (IntConst i)         = integer i
  pretty (FloatConst srep)    = text srep
  pretty (BoolConst True)     = text "true" 
  pretty (BoolConst False)    = text "false"


instance Pretty UnaryOp where
  pretty PreIncOp             = text "++"
  pretty PreDecOp             = text "--"
  pretty PostIncOp            = text "++"
  pretty PostDecOp            = text "--"
  pretty PlusOp               = char '+'
  pretty MinusOp              = char '-'
  pretty LNotOp               = char '!'
  pretty NotOp                = char '~'

instance Pretty BinaryOp where
  pretty MulOp                = char '*'
  pretty DivOp                = char '/'
  pretty RemainderOp          = char '%'
  pretty AddOp                = char '+'
  pretty SubOp                = char '-'
  pretty ShiftLOp             = text "<<"
  pretty ShiftROp             = text ">>"
  pretty LtOp                 = char '<'
  pretty GtOp                 = char '>'
  pretty LteOp                = text "<="
  pretty GteOp                = text ">="
  pretty EqOp                 = text "=="
  pretty NeqOp                = text "!="
  pretty AndOp                = char '&'
  pretty XorOp                = char '^'
  pretty OrOp                 = char '|'
  pretty LandOp               = text "&&"
  pretty LxorOp               = text "^^"
  pretty LorOp                = text "||"       

instance Pretty AssignOp where
  pretty AssignOp             = char '='
  pretty MulAssign            = text "*="
  pretty DivAssign            = text "/="
  pretty ModAssign            = text "%="
  pretty AddAssign            = text "+="
  pretty SubAssign            = text "-="
  pretty LShiftAssign         = text "<<="
  pretty RShiftAssign         = text ">>="
  pretty AndAssign            = text "&="
  pretty XorAssign            = text "^="
  pretty OrAssign             = text "|="


instance Pretty ParamQual where
  pretty In                   = text "in"
  pretty Out                  = text "out"
  pretty Inout                = text "inout"
  
instance Pretty TypeQual where
  pretty Const                = text "const"
  pretty Attribute            = text "const"
  pretty (Varying [])         = text "varying"
  pretty (Varying xs)         = (hsep $ map pretty xs) <+> text "varying"
  pretty Uniform              = text "uniform"
  
instance Pretty VaryingQual where
  pretty Centroid             = text "centroid"
  pretty Invariant            = text "invariant"
                   
                     
instance Pretty ScalarTypeSpec where
  pretty SlVoid               = text "void"
  pretty SlFloat              = text "float"
  pretty SlInt                = text "int"
  pretty SlBool               = text "bool"
  pretty Vec2                 = text "vec2"
  pretty Vec3                 = text "vec3"
  pretty Vec4                 = text "vec4"
  pretty BVec2                = text "bvec2"
  pretty BVec3                = text "bvec3"
  pretty BVec4                = text "bvec4"
  pretty IVec2                = text "ivec2"
  pretty IVec3                = text "ivec4"
  pretty IVec4                = text "ivec4"
  pretty Mat2                 = text "mat2"
  pretty Mat3                 = text "mat3"
  pretty Mat4                 = text "mat4"
  pretty Mat2x2               = text "mat2x2"
  pretty Mat2x3               = text "mat2x3"
  pretty Mat2x4               = text "mat2x4"
  pretty Mat3x2               = text "mat3x2"
  pretty Mat3x3               = text "mat3x3"
  pretty Mat3x4               = text "mat3x4"
  pretty Mat4x2               = text "mat4x2"
  pretty Mat4x3               = text "mat4x3"
  pretty Mat4x4               = text "mat4x4"
  pretty Sampler1D            = text "sampler1D"
  pretty Sampler2D            = text "sampler2D"
  pretty Sampler3D            = text "sampler3D"
  pretty SamplerCube          = text "samplerCube"
  pretty Sampler1DShadow      = text "sampler1DShadow"
  pretty Sampler2DShadow      = text "sampler2DShadow"
  pretty (StructType _s)      = text "__TODO__"
  pretty (TypeName s)         = text s
                         
       