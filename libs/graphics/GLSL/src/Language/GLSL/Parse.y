--------------------------------------------------------------------------------
-- |
-- Module      :  Language.GLSL.Parse
-- Copyright   :  (c) Stephen Tetley 2012
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Lexer
-- 
--------------------------------------------------------------------------------

{


module Language.GLSL.Parse 
  (

    parseGlsl
    
  ) where

import Language.GLSL.Lex
import Language.GLSL.ParseMonad
import Language.GLSL.Syntax
import Language.GLSL.Token


import Control.Monad.Identity

}



%name glslParser translation_unit

%lexer { glslLex } { L _ Tk_EOF }
%monad { ParseM } { >>= } { return }
%error { parseError }


%tokentype { Lexeme }

%token 
  ATTRIBUTE           { L _ Tk_kw_attribute         }
  CONST               { L _ Tk_kw_const             }
  BOOL                { L _ Tk_kw_bool              }
  FLOAT               { L _ Tk_kw_float             }
  INT                 { L _ Tk_kw_int               }
  BREAK               { L _ Tk_kw_break             }
  CONTINUE            { L _ Tk_kw_continue          }
  DO                  { L _ Tk_kw_do                }
  ELSE                { L _ Tk_kw_else              }
  FOR                 { L _ Tk_kw_for               }
  IF                  { L _ Tk_kw_if                }
  DISCARD             { L _ Tk_kw_discard           }
  RETURN              { L _ Tk_kw_return            }
  BVEC2               { L _ Tk_kw_bvec2             }
  BVEC3               { L _ Tk_kw_bvec3             } 
  BVEC4               { L _ Tk_kw_bvec4             }
  IVEC2               { L _ Tk_kw_ivec2             }
  IVEC3               { L _ Tk_kw_ivec3             }
  IVEC4               { L _ Tk_kw_ivec4             } 
  VEC2                { L _ Tk_kw_vec2              }
  VEC3                { L _ Tk_kw_vec3              }
  VEC4                { L _ Tk_kw_vec4              }
  MAT2                { L _ Tk_kw_mat2              }
  MAT3                { L _ Tk_kw_mat3              }
  MAT4                { L _ Tk_kw_mat4              }
  IN                  { L _ Tk_kw_in                }
  OUT                 { L _ Tk_kw_out               }
  INOUT               { L _ Tk_kw_inout             }  
  UNIFORM             { L _ Tk_kw_uniform           }
  VARYING             { L _ Tk_kw_varying           }
  CENTROID            { L _ Tk_kw_centroid          }
  MAT2X2              { L _ Tk_kw_mat2x2            }
  MAT2X3              { L _ Tk_kw_mat2x3            }
  MAT2X4              { L _ Tk_kw_mat2x4            }
  MAT3X2              { L _ Tk_kw_mat3x2            }
  MAT3X3              { L _ Tk_kw_mat3x3            }
  MAT3X4              { L _ Tk_kw_mat3x4            }
  MAT4X2              { L _ Tk_kw_mat4x2            }
  MAT4X3              { L _ Tk_kw_mat4x3            }
  MAT4X4              { L _ Tk_kw_mat4x4            }
  SAMPLER1D           { L _ Tk_kw_sampler1D         } 
  SAMPLER2D           { L _ Tk_kw_sampler2D         } 
  SAMPLER3D           { L _ Tk_kw_sampler3D         } 
  SAMPLERCUBE         { L _ Tk_kw_samplerCube       }
  SAMPLER1DSHADOW     { L _ Tk_kw_sampler1DShadow   } 
  SAMPLER2DSHADOW     { L _ Tk_kw_sampler2DShadow   }
  STRUCT              { L _ Tk_kw_struct            }
  VOID                { L _ Tk_kw_void              }
  WHILE               { L _ Tk_kw_while             }

  IDENTIFIER          { L _ (Tk_ident $$)           }
  TYPE_NAME           { L _ (Tk_ident $$)           }   
  FLOATCONSTANT       { L _ (Tk_lit_float $$)       }
  INTCONSTANT         { L _ (Tk_lit_int $$)         }
  BOOLCONSTANT        { L _ (Tk_lit_bool $$)        }
  
  LEFT_OP             { L _ Tk_left_op              }
  RIGHT_OP            { L _ Tk_right_op             }
   
  INC_OP              { L _ Tk_inc_op               }
  DEC_OP              { L _ Tk_dec_op               }
  LE_OP               { L _ Tk_le_op                }
  GE_OP               { L _ Tk_ge_op                }
  EQ_OP               { L _ Tk_eq_op                }
  NE_OP               { L _ Tk_ne_op                }
  
  AND_OP              { L _ Tk_and_op               }
  XOR_OP              { L _ Tk_xor_op               }
  OR_OP               { L _ Tk_or_op                } 
  MUL_ASSIGN          { L _ Tk_mul_assign           }
  DIV_ASSIGN          { L _ Tk_div_assign           }
  ADD_ASSIGN          { L _ Tk_add_assign           }
  MOD_ASSIGN          { L _ Tk_mod_assign           }
  LEFT_ASSIGN         { L _ Tk_left_assign          }
  RIGHT_ASSIGN        { L _ Tk_right_assign         }
  AND_ASSIGN          { L _ Tk_and_assign           }
  XOR_ASSIGN          { L _ Tk_xor_assign           }
  OR_ASSIGN           { L _ Tk_or_assign            }
  SUB_ASSIGN          { L _ Tk_sub_assign           }
  
  LEFT_PAREN          { L _ Tk_p_left_paren         }
  RIGHT_PAREN         { L _ Tk_p_right_paren        }
  LEFT_BRACKET        { L _ Tk_p_left_bracket       }
  RIGHT_BRACKET       { L _ Tk_p_right_bracket      }   
  LEFT_BRACE          { L _ Tk_p_left_brace         }
  RIGHT_BRACE         { L _ Tk_p_right_brace        }
  DOT                 { L _ Tk_p_dot                }
  
  COMMA               { L _ Tk_p_comma              }
  COLON               { L _ Tk_p_colon              }
  EQUAL               { L _ Tk_p_equal              }
  SEMICOLON           { L _ Tk_p_semicolon          }
  BANG                { L _ Tk_p_bang               }
  DASH                { L _ Tk_p_dash               }
  TILDE               { L _ Tk_p_tilde              }
  PLUS                { L _ Tk_p_plus               }
  STAR                { L _ Tk_p_star               }
  SLASH               { L _ Tk_p_slash              }
  PERCENT             { L _ Tk_p_percent            }

  LEFT_ANGLE          { L _ Tk_p_left_angle         }
  RIGHT_ANGLE         { L _ Tk_p_right_angle        }
  VERTICAL_BAR        { L _ Tk_p_vertical_bar       }
  CARET               { L _ Tk_p_caret              }
  AMPERSAND           { L _ Tk_p_ampersand          }
  QUESTION            { L _ Tk_p_question           }

  INVARIANT           { L _ Tk_kw_invariant         }


%%


variable_identifier :: { Ident }
  : IDENTIFIER                { $1 }

primary_expression :: { Expr }
  : variable_identifier                 { VarExpr $1 }
  | INTCONSTANT                         { ConstantExpr (IntConst $1) }
  | FLOATCONSTANT                       { ConstantExpr (FloatConst $1) }
  | BOOLCONSTANT                        { ConstantExpr (BoolConst $1) }
  | LEFT_PAREN expression RIGHT_PAREN   { $2 }

  
-- Note - should be a FIELD_SELECTION after DOT, but the lexer does
-- not have access to structures to decide when an identifier
-- is a field.
  
postfix_expression :: { Expr }
  : primary_expression                  { $1 }
  | postfix_expression LEFT_BRACKET integer_expression RIGHT_BRACKET
                                        { ArrayAccessExpr $1 $3 }
  | function_call                       { $1 }
  | postfix_expression DOT IDENTIFIER
                                        { FieldAccessExpr $1 $3 }
  | postfix_expression INC_OP           { UnaryExpr PostIncOp $1 }
  | postfix_expression DEC_OP           { UnaryExpr PostDecOp $1 }

integer_expression :: { Expr }
  : expression                { $1 }

function_call :: { Expr }
  : function_call_or_method   { $1 }

function_call_or_method :: { Expr }
  : function_call_generic               { $1 }
  | postfix_expression DOT function_call_generic
                                        { MethodAccessExpr $1 $3 }

function_call_generic :: { Expr }
  : function_call_header_with_parameters RIGHT_PAREN
                              { (\(s,hs) -> FunCallExpr s (toListH hs)) $1 }
  | function_call_header_no_parameters RIGHT_PAREN
                              { (\(s,hs) -> FunCallExpr s (toListH hs)) $1 }

function_call_header_no_parameters :: { (Ident, H Expr) }
  : function_call_header VOID           { ($1, emptyH) }
  | function_call_header                { ($1, emptyH) }

function_call_header_with_parameters :: { (Ident, H Expr) }
  : function_call_header assignment_expression    { ($1, wrapH $2) }
  | function_call_header_with_parameters COMMA assignment_expression
                                                  { (\(s,hs) a -> (s,hs `snocH` a)) $1 $3 }
function_call_header :: { Ident }
  : function_identifier LEFT_PAREN                { $1 }

-- Changed from original grammar as FIELD_SELECTION is not
-- distinguished from IDENTIFIER

function_identifier :: { Ident }
  : type_specifier            { constructorIdent $1 }
  | IDENTIFIER                { $1 }

unary_expression :: { Expr }
  : postfix_expression                { $1 }
  | INC_OP unary_expression           { UnaryExpr PreIncOp $2 }
  | DEC_OP unary_expression           { UnaryExpr PreDecOp $2 }
  | unary_operator unary_expression   { UnaryExpr $1 $2 }

unary_operator :: { UnaryOp }
  : PLUS                      { PlusOp }
  | DASH                      { MinusOp }
  | BANG                      { LNotOp }
  | TILDE                     { NotOp }

multiplicative_expression :: { Expr }
  : unary_expression          { $1 }
  | multiplicative_expression STAR unary_expression
                              { BinaryExpr MulOp $1 $3 }
  | multiplicative_expression SLASH unary_expression
                              { BinaryExpr DivOp $1 $3 }
  | multiplicative_expression PERCENT unary_expression
                              { BinaryExpr RemainderOp $1 $3 }

additive_expression :: { Expr }
  : multiplicative_expression
                              { $1 }
  | additive_expression PLUS multiplicative_expression
                              { BinaryExpr AddOp $1 $3 }
  | additive_expression DASH multiplicative_expression
                              { BinaryExpr SubOp $1 $3 }

shift_expression :: { Expr }
  : additive_expression       { $1 }
  | shift_expression LEFT_OP additive_expression
                              { BinaryExpr ShiftLOp $1 $3 }
  | shift_expression RIGHT_OP additive_expression
                              { BinaryExpr ShiftROp $1 $3 }

relational_expression :: { Expr }
  : shift_expression        { $1 }
  | relational_expression LEFT_ANGLE shift_expression
                            { BinaryExpr LtOp $1 $3 }
  | relational_expression RIGHT_ANGLE shift_expression
                            { BinaryExpr GtOp $1 $3 }
  | relational_expression LE_OP shift_expression
                            { BinaryExpr LteOp $1 $3 }
  | relational_expression GE_OP shift_expression
                            { BinaryExpr GteOp $1 $3 }

equality_expression :: { Expr }
  : relational_expression   { $1 }
  | equality_expression EQ_OP relational_expression
                            { BinaryExpr EqOp $1 $3 }
  | equality_expression NE_OP relational_expression
                            { BinaryExpr NeqOp $1 $3 }

and_expression :: { Expr }
  : equality_expression     { $1 }
  | and_expression AMPERSAND equality_expression
                            { BinaryExpr AndOp $1 $3 }

exclusive_or_expression :: { Expr }
  : and_expression          { $1 }
  | exclusive_or_expression CARET and_expression
                            { BinaryExpr XorOp $1 $3 }

inclusive_or_expression :: { Expr }
  : exclusive_or_expression
                            { $1 }
  | inclusive_or_expression VERTICAL_BAR exclusive_or_expression
                            { BinaryExpr OrOp $1 $3 }

logical_and_expression :: { Expr }
  : inclusive_or_expression
                            { $1 }
  | logical_and_expression AND_OP inclusive_or_expression
                            { BinaryExpr LandOp $1 $3 }

logical_xor_expression :: { Expr }
  : logical_and_expression
                            { $1 }
  | logical_xor_expression XOR_OP logical_and_expression
                            { BinaryExpr LxorOp $1 $3 }


logical_or_expression :: { Expr }
  : logical_xor_expression
                            { $1 }
  | logical_or_expression OR_OP logical_xor_expression
                            { BinaryExpr LorOp $1 $3 }

conditional_expression :: { Expr }
  : logical_or_expression   { $1 }
  | logical_or_expression QUESTION expression COLON assignment_expression
                            { TernaryExpr $1 $3 $5 }

assignment_expression :: { Expr }
  : conditional_expression  { $1 }
  | unary_expression assignment_operator assignment_expression
                            { AssignExpr $2 $1 $3 }


assignment_operator :: { AssignOp }
  : EQUAL                     { AssignOp }
  | MUL_ASSIGN                { MulAssign }
  | DIV_ASSIGN                { DivAssign }
  | MOD_ASSIGN                { ModAssign }
  | ADD_ASSIGN                { AddAssign }
  | SUB_ASSIGN                { SubAssign }
  | LEFT_ASSIGN               { LShiftAssign }
  | RIGHT_ASSIGN              { RShiftAssign }
  | AND_ASSIGN                { AndAssign }
  | XOR_ASSIGN                { XorAssign }
  | OR_ASSIGN                 { OrAssign }


expression :: { Expr }
  : expression_list     { CommaExpr $ toListH $1 }


expression_list :: { H Expr }
  : assignment_expression     { wrapH $1 }
  | expression_list COMMA assignment_expression
                              { $1 `snocH` $3 }


constant_expression :: { Expr }
  : conditional_expression    { $1 }

declaration :: { Declaration }
  : function_prototype SEMICOLON      { FunProtoDecl $1 }
  | init_declarator_list SEMICOLON    { InitDeclr $1 }

function_prototype :: { FunProto }
  : function_declarator RIGHT_PAREN
                    { (\((ty,name),se) -> FunProto ty name (toListH se)) $1 }

function_declarator :: { ((FullType,Ident), H ParamDecl) }
  : function_header                   { ($1, emptyH) }
  | function_header_with_parameters   { $1 }

function_header_with_parameters :: { ((FullType,Ident), H ParamDecl) }
  : function_header parameter_declaration
                                      { ($1, wrapH $2) }
  | function_header_with_parameters COMMA parameter_declaration
                                      { (\(t,hs) a -> (t, hs `snocH` a)) $1 $3  }

function_header :: { (FullType,Ident) }
  : fully_specified_type IDENTIFIER LEFT_PAREN
                                      { ($1,$2) }

parameter_declarator :: { ParamDeclarator }
  : type_specifier IDENTIFIER         { ParamScalarDeclr $1 $2 }
  | type_specifier IDENTIFIER LEFT_BRACKET constant_expression RIGHT_BRACKET
                                      { ParamArrayDeclr $1 $2 $4 }

parameter_declaration :: { ParamDecl }
  : type_qualifier parameter_qualifier parameter_declarator
                              { Declarator (Just $1) $2 $3 }
  | parameter_qualifier parameter_declarator
                              { Declarator Nothing $1 $2 }
  | type_qualifier parameter_qualifier parameter_type_specifier
                              { Specifier (Just $1) $2 $3 }
  | parameter_qualifier parameter_type_specifier
                              { Specifier Nothing $1 $2 }


parameter_qualifier :: { ParamQual }
  : IN                        { In }
  | OUT                       { Out }
  | INOUT                     { Inout }

parameter_type_specifier :: { TypeSpec }
  : type_specifier    { $1 }


init_declarator_list :: { SingleDeclaration }
  : single_declaration        { $1 }
  | init_declarator_list COMMA IDENTIFIER
                              { $1 `snocDeclr` (ScalarDeclr $3 Nothing) }
  | init_declarator_list COMMA IDENTIFIER LEFT_BRACKET RIGHT_BRACKET
                              { $1 `snocDeclr` (ArrayDeclr $3 Nothing Nothing) }
  | init_declarator_list COMMA IDENTIFIER LEFT_BRACKET constant_expression
                                        RIGHT_BRACKET
                              { $1 `snocDeclr` (ArrayDeclr $3 (Just $5) Nothing) }
  | init_declarator_list COMMA IDENTIFIER LEFT_BRACKET
                                        RIGHT_BRACKET EQUAL initializer
                              { $1 `snocDeclr` (ArrayDeclr $3 Nothing (Just $7)) }
  | init_declarator_list COMMA IDENTIFIER LEFT_BRACKET constant_expression
                                        RIGHT_BRACKET EQUAL initializer
                              { $1 `snocDeclr` (ArrayDeclr $3 (Just $5) (Just $8)) }
  | init_declarator_list COMMA IDENTIFIER EQUAL initializer
                              { $1 `snocDeclr` (ScalarDeclr $3 (Just $5)) }

single_declaration :: { SingleDeclaration }
  : fully_specified_type
                    { SingleDeclaration $1 [] }
  | fully_specified_type IDENTIFIER
                    { SingleDeclaration $1 [ScalarDeclr $2 Nothing] }
  | fully_specified_type IDENTIFIER LEFT_BRACKET RIGHT_BRACKET
                    { SingleDeclaration $1 [ArrayDeclr $2 Nothing Nothing] }
  | fully_specified_type IDENTIFIER LEFT_BRACKET constant_expression
                                        RIGHT_BRACKET
                    { SingleDeclaration $1 [ArrayDeclr $2 (Just $4) Nothing] }
  | fully_specified_type IDENTIFIER LEFT_BRACKET RIGHT_BRACKET EQUAL initializer
                    { SingleDeclaration $1 [ArrayDeclr $2 Nothing (Just $6)] }
  | fully_specified_type IDENTIFIER LEFT_BRACKET constant_expression
                                        RIGHT_BRACKET EQUAL initializer
                    { SingleDeclaration $1 [ArrayDeclr $2 (Just $4) (Just $7)] }
  | fully_specified_type IDENTIFIER EQUAL initializer
                    { SingleDeclaration $1 [ScalarDeclr $2 (Just $4)]  }
  | INVARIANT IDENTIFIER
                    { InvariantDeclaration $2 [] }

fully_specified_type :: { FullType }
  : type_specifier                      { (Nothing, $1) }
  | type_qualifier type_specifier       { (Just $1, $2) }


type_qualifier :: { TypeQual }
  : CONST                               { Const }
  | ATTRIBUTE                           { Attribute }
  | VARYING                             { Varying [] }
  | CENTROID VARYING                    { Varying [Centroid] }
  | INVARIANT VARYING                   { Varying [Invariant] }
  | INVARIANT CENTROID VARYING          { Varying [Invariant, Centroid] }
  | UNIFORM                             { Uniform }

type_specifier :: { TypeSpec }
  : type_specifier_nonarray   { ScalarType $1 }
  | type_specifier_nonarray LEFT_BRACKET constant_expression RIGHT_BRACKET
                              { ArrayType $1 $3 }

type_specifier_nonarray :: { ScalarTypeSpec }
  : VOID                { SlVoid }
  | FLOAT               { SlFloat }
  | INT                 { SlInt }
  | BOOL                { SlBool }
  | VEC2                { Vec2 }
  | VEC3                { Vec3 }
  | VEC4                { Vec4 }
  | BVEC2               { BVec2 }
  | BVEC3               { BVec3 }
  | BVEC4               { BVec4 }
  | IVEC2               { IVec2 }
  | IVEC3               { IVec3 }
  | IVEC4               { IVec4 }
  | MAT2                { Mat2 }
  | MAT3                { Mat3 }
  | MAT4                { Mat4 }
  | MAT2X2              { Mat2x2 }
  | MAT2X3              { Mat2x3 }
  | MAT2X4              { Mat2x4 }
  | MAT3X2              { Mat3x2 }
  | MAT3X3              { Mat3x3 }
  | MAT3X4              { Mat3x4 }
  | MAT4X2              { Mat4x2 }
  | MAT4X3              { Mat4x3 }
  | MAT4X4              { Mat4x4 }
  | SAMPLER1D           { Sampler1D }
  | SAMPLER2D           { Sampler2D }
  | SAMPLER3D           { Sampler3D }
  | SAMPLERCUBE         { SamplerCube }
  | SAMPLER1DSHADOW     { Sampler1DShadow }
  | SAMPLER2DSHADOW     { Sampler2DShadow }
  | struct_specifier    { StructType $1 }
  | TYPE_NAME           { TypeName $1 }

struct_specifier :: { StructSpecifier }
  : STRUCT IDENTIFIER LEFT_BRACE struct_declaration_list RIGHT_BRACE
                              { StructSpec (Just $2) (toListH $4) }
  | STRUCT LEFT_BRACE struct_declaration_list RIGHT_BRACE
                              { StructSpec Nothing (toListH $3) }

struct_declaration_list :: { H StructDeclaration }
  : struct_declaration                  { wrapH $1 }
  | struct_declaration_list struct_declaration
                                        { $1 `snocH` $2 }

struct_declaration :: { StructDeclaration }
  : type_specifier struct_declarator_list SEMICOLON
                              { StructDeclaration $1 (toListH $2)  }

struct_declarator_list :: { H StructDeclarator }
  : struct_declarator         { wrapH $1 }
  | struct_declarator_list COMMA struct_declarator
                              { $1 `snocH` $3 }

struct_declarator :: { StructDeclarator }
  : IDENTIFIER                { StructScalarDeclarator $1 }
  | IDENTIFIER LEFT_BRACKET constant_expression RIGHT_BRACKET
                              { StructArrayDeclarator $1 $3 }

initializer :: { Expr }
  : assignment_expression     { $1 }

declaration_statement :: { Stmt }
  : declaration               { DeclStmt $1 }

statement :: { Stmt }
  : compound_statement        { $1 }
  | simple_statement          { $1 }

simple_statement :: { Stmt }
  : declaration_statement     { $1 }
  | expression_statement      { $1 }
  | selection_statement       { $1 }
  | iteration_statement       { $1 }
  | jump_statement            { $1 }

compound_statement :: { Stmt }
  : LEFT_BRACE RIGHT_BRACE                        { CompoundStmt [] }
  | LEFT_BRACE statement_list RIGHT_BRACE         { CompoundStmt $ toListH $2 }

statement_no_new_scope :: { Stmt }
  : compound_statement_no_new_scope     { $1 }
  | simple_statement                    { $1 }

compound_statement_no_new_scope :: { Stmt }
  : LEFT_BRACE RIGHT_BRACE                        { CompoundStmt [] }
  | LEFT_BRACE statement_list RIGHT_BRACE         { CompoundStmt $ toListH $2 }



statement_list :: { H Stmt }
  : statement                           { wrapH $1 }
  | statement_list statement            { $1 `snocH` $2 }

expression_statement :: { Stmt }
  : SEMICOLON                 { ExprStmt Nothing }
  | expression SEMICOLON      { ExprStmt (Just $1) }
  
selection_statement :: { Stmt }
  : IF LEFT_PAREN expression RIGHT_PAREN selection_rest_statement
                              { mkIfStmt $3 $5 }

selection_rest_statement :: { (Stmt, Maybe Stmt) }
  : statement ELSE statement  { ($1, Just $3) }
  | statement                 { ($1, Nothing) }
  
condition :: { Expr }
  : expression                { $1 }
  | fully_specified_type IDENTIFIER EQUAL initializer
                              { NewVar $1 $2 $4 }

iteration_statement :: { Stmt }
  : WHILE LEFT_PAREN condition RIGHT_PAREN statement_no_new_scope
                              { While $3 $5 }
  | DO statement WHILE LEFT_PAREN expression RIGHT_PAREN SEMICOLON
                              { DoWhile $2 $5 }
  | FOR LEFT_PAREN for_init_statement for_rest_statement RIGHT_PAREN
                                        statement_no_new_scope
                              { mkFor (unwrapExpr $3) $4 $6 }
                                        
for_init_statement :: { Stmt }
  : expression_statement      { $1 }
  | declaration_statement     { $1 }

conditionopt :: { Expr }
  : condition                 { $1 }

for_rest_statement :: { (Expr, Maybe Expr) }
  : conditionopt SEMICOLON              { ($1,Nothing) }
  | conditionopt SEMICOLON expression   { ($1,Just $3) }

jump_statement :: { Stmt }
  : CONTINUE SEMICOLON                  { Continue }
  | BREAK SEMICOLON                     { Break }
  | RETURN SEMICOLON                    { Return Nothing }
  | RETURN expression SEMICOLON         { Return (Just $2) }
  | DISCARD SEMICOLON                   { Discard }


translation_unit :: { TranslUnit }
  : global_decl_list                    { TranslUnit $ toListH $1 }

global_decl_list :: { H ExtDeclaration }
  : external_declaration                          { wrapH $1 }
  | global_decl_list external_declaration         { $1 `snocH` $2 }

external_declaration :: { ExtDeclaration }
  : function_definition       { ExtFunDef $1 }
  | declaration               { ExtDeclaration $1 }

function_definition :: { FunDef }
  : function_prototype compound_statement_no_new_scope
                              { FunDef $1 $2 }


{


-- Hughes List

type H a = [a] -> [a]

wrapH :: a -> H a
wrapH = (:)

snocH :: H a -> a -> H a
snocH f a = f . (a:)

emptyH :: H a
emptyH = id

toListH :: H a -> [a]
toListH f = f $ []

snocDeclr :: SingleDeclaration -> DeclrElement -> SingleDeclaration
snocDeclr (SingleDeclaration ty xs)   x = SingleDeclaration ty (xs++[x])
snocDeclr (InvariantDeclaration s xs) x = InvariantDeclaration s (xs++[x])

parseGlsl :: FilePath -> String -> Either String TranslUnit
parseGlsl path contents  =
  case runIdentity (runParseT glslParser path contents) of
    Left (ParseErr err) -> Left err
    Right ans           -> Right ans


unwrapExpr :: Stmt -> Expr
unwrapExpr (ExprStmt (Just e))  = e
unwrapExpr _                    = error $ "fail"

mkIfStmt :: Expr -> (Stmt, Maybe Stmt) -> Stmt
mkIfStmt condE (thenS,opt_elseS) = IfStmt condE thenS opt_elseS

mkFor :: Expr -> (Expr, Maybe Expr) -> Stmt -> Stmt
mkFor initE (condE,opt_loopE) bodyS = For initE condE opt_loopE bodyS

constructorIdent :: TypeSpec -> Ident
constructorIdent ts = case ts of
    ScalarType ty -> fn ty
    ArrayType ty _ -> fn ty
  where     
    fn Vec2             = "vec2"
    fn Vec3             = "vec3"
    fn Vec4             = "vec4"
    fn BVec2            = "bvec2"
    fn BVec3            = "bvec3"
    fn BVec4            = "bvec4"
    fn IVec2            = "ivec2"
    fn IVec3            = "ivec3"
    fn IVec4            = "ivec4"
    fn Mat2             = "mat2"
    fn Mat3             = "mat3"
    fn Mat4             = "mat4"
    fn Mat2x2           = "mat2x2"
    fn Mat2x3           = "mat2x3"
    fn Mat2x4           = "mat2x4"
    fn Mat3x2           = "mat3x2"
    fn Mat3x3           = "mat3x3"
    fn Mat3x4           = "mat3x4"
    fn Mat4x2           = "mat4x2"
    fn Mat4x3           = "mat4x3"
    fn Mat4x4           = "mat4x4"
    fn z                = error $ "constructorIdent on " ++ show z

 


}  
