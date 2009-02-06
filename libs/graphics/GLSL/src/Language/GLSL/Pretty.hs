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

import qualified Data.Foldable as F
import Data.Sequence hiding (empty) 
import Text.PrettyPrint.Leijen

infixl 7 #

( # ) :: a -> (a -> b) -> b 
x # f = f x

data Fixity = Prefix | Postfix 
  deriving (Eq,Show) 

dfoldseq :: (a -> Doc) -> (Doc -> Doc -> Doc) -> Seq a -> Doc
dfoldseq f g se = case viewl se of
    EmptyL  -> empty
    z :< sz -> F.foldl' (\a e -> a `g` (f e)) (f z) sz

commaSpace :: Doc -> Doc -> Doc
commaSpace a b = a <> comma <> space <> b

semiLine :: Doc -> Doc -> Doc
semiLine a b = a <> semi <$> b


semiSpace :: Doc -> Doc -> Doc
semiSpace a b = a <> semi <+> b


optInitializer :: Maybe Expr -> (Doc -> Doc)
optInitializer Nothing  = (<> empty)
optInitializer (Just e) = (<+> equals <+> pretty e) 



arraySize :: Maybe Expr -> Doc
arraySize Nothing  = brackets empty
arraySize (Just e) = brackets $ pretty e


instance Pretty TranslUnit where
  pretty (TranslUnit se) = dfoldseq pretty (<$>) se
  

instance Pretty GblDecl where
  pretty (GblFunDef e) = pretty e
  pretty (GblDecl e)   = pretty e 
  
instance Pretty FunDef where
  pretty (FunDef proto stmt) = pretty proto <+> braces (pretty stmt)

instance Pretty Constant where
  pretty (IntConst i)         = integer i
  pretty (FloatConst srep)    = text srep
  pretty (BoolConst True)     = text "true" 
  pretty (BoolConst False)    = text "false"
  
  
instance Pretty Decl where
  pretty (FunProtoDecl proto) = pretty proto <> semi
  pretty (InitDeclr declrs)   = pretty declrs <> semi   
            
instance Pretty Declrs where
  pretty (Declr ty se)             = 
      ppFullType ty <+> dfoldseq pretty commaSpace se
  pretty (InvariantDeclr ident se) = 
      text "invariant" <+> text ident <+> dfoldseq pretty commaSpace se

instance Pretty DeclrElement where 
  pretty (ScalarDeclr ident oin)      = text ident # optInitializer oin                   
  pretty (ArrayDeclr  ident oas oin)  = 
      text ident <> arraySize oas # optInitializer oin
           
instance Pretty Struct where 
  pretty (Struct Nothing se)     = text "struct" <+> structbody se
  pretty (Struct (Just name) se) = 
      text "struct" <+> text name <+> structbody se

structbody :: Seq StructDeclr -> Doc
structbody = braces . dfoldseq pretty semiLine

instance Pretty StructDeclr where
  pretty (StructDeclr ty se) = pretty ty <+> dfoldseq pretty commaSpace se

instance Pretty StructDeclrElement where 
  pretty (StructScalarDeclr ident)   = text ident                   
  pretty (StructArrayDeclr  ident e) = text ident <> brackets (pretty e)
                          
                          
instance Pretty UnaryOp where
  pretty PreIncOp             = text "++"
  pretty PreDecOp             = text "--"
  pretty PostIncOp            = text "++"
  pretty PostDecOp            = text "--"
  pretty PlusOp               = char '+'
  pretty MinusOp              = char '-'
  pretty LNotOp               = char '!'
  pretty NotOp                = char '~'

fixity :: UnaryOp -> Fixity
fixity PreIncOp             = Prefix
fixity PreDecOp             = Prefix
fixity PostIncOp            = Postfix
fixity PostDecOp            = Postfix
fixity PlusOp               = Prefix
fixity MinusOp              = Prefix
fixity LNotOp               = Prefix
fixity NotOp                = Prefix
  
  
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

instance Pretty Expr where
  pretty (ConstantExpr e)         = pretty e
  pretty ContructorExpr           = pretty "ContructorExpr - error"
  pretty (VarExpr ident)          = text ident
  pretty (NewVar ty ident e)      = ppFullType ty <+> text ident <+> pretty e          
  pretty (AssignExpr op le re)    = pretty le <+> pretty op <+> pretty re 
  pretty (UnaryExpr op e)         = case fixity op of
                                    Prefix -> pretty op <> pretty e
                                    _      -> pretty e <> pretty op
  pretty (BinaryExpr op le re)    = pretty le <+> pretty op <+> pretty re
  pretty (ArrayAccessExpr e ae)   = pretty e <> brackets (pretty ae)
  pretty (FieldAccessExpr e s)    = pretty e <> dot <> text s
  pretty (MethodAccessExpr e fc)  = pretty e <> dot <> pretty fc                    
  pretty (FunCallExpr ident se)   = 
      pretty ident <> parens (dfoldseq pretty commaSpace se)
  pretty (CommaExpr se)           = dfoldseq pretty commaSpace se
  pretty (TernaryExpr ce te fe)   = pretty ce <+> char '?' <+> pretty te
                                              <+> colon    <+> pretty fe 
                        
instance Pretty FunProto where
  pretty (FunProto ty ident se)   = 
      ppFullType ty <> text ident <> parens (dfoldseq pretty commaSpace se)

                         
instance Pretty ParamDecl where
  pretty (Declarator otq pql pd)  = 
      prefixTypeQual otq <> pretty pql <+> pretty pd
  pretty (Specifier otq pql ts)   = 
      prefixTypeQual otq <> pretty pql <+> pretty ts

ppFullType :: FullType -> Doc
ppFullType (Nothing, ts) = pretty ts
ppFullType (Just tq, ts) = pretty tq <+> pretty ts

prefixTypeQual :: Maybe TypeQual -> Doc
prefixTypeQual (Just tq) = pretty tq <> space
prefixTypeQual Nothing   = empty                          

instance Pretty ParamDeclr where 
  pretty (ParamScalarDeclr ts ident)   = pretty ts <+> text ident
  pretty (ParamArrayDeclr  ts ident e) = 
      pretty ts <+> text ident <> brackets (pretty e)
                         
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
                   
instance Pretty TypeSpec where
  pretty (ScalarType ty)      = pretty ty
  pretty (ArrayType  ty e)    = pretty ty <> brackets (pretty e)
  
  
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
  pretty (StructType s)       = pretty s
  pretty (TypeName s)         = text s
                         
instance Pretty Stmt where
  pretty (CompoundStmt se)        = braces $ dfoldseq pretty semiLine se
  pretty (DeclStmt decl)          = pretty decl
  pretty (ExprStmt Nothing)       = empty -- correct?
  pretty (ExprStmt (Just e))      = pretty e
  pretty (IfStmt e ts Nothing)    = ifThen e ts
  pretty (IfStmt e ts (Just fs))  = ifThen e ts <$> text "else" <+> pretty fs
  pretty (For ini cond oloop s)   = 
      text "for" <+> parens (pretty ini `semiSpace` pretty cond
                                        `semiSpace` optLoop oloop)
                 <$> pretty s
    where optLoop = maybe empty pretty
                     
  pretty (While e s)              =     text "while" <+> parens (pretty e) 
                                    <$> pretty s    
  pretty (DoWhile s e)            =     text "do" <+> (pretty s)
                                    <$> text "while" <+> parens (pretty e) <> semi           
  pretty Continue                 = text "continue"
  pretty Break                    = text "break"
  pretty (Return Nothing)         = text "return"
  pretty (Return (Just e))        = text "return" <+> pretty e
  pretty Discard                  = text "discard"

ifThen :: Expr -> Stmt -> Doc
ifThen e s =   text "if" <+> parens (pretty e) <$> pretty s
                                
  