--------------------------------------------------------------------------------
-- |
-- Module      :  Language.GLSL.Parse
-- Copyright   :  (c) Stephen Tetley 2009
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
{-# OPTIONS -Wall #-}

module Language.GLSL.Parse where

import Language.GLSL.Lex
import Language.GLSL.ParseMonad
import Language.GLSL.Syntax
import Language.GLSL.Tokens


import Control.Monad.Identity



-- parseGlsl :: FilePath -> String -> Integer
parseGlsl path contents  = 
  case runIdentity (runParseT glslParser path contents) of
    Left (ParseErr err) -> error err
    Right a             -> a 

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
  TYPE_NAME           { L _ (Tk_tyname $$)          }   
  FLOATCONSTANT       { L _ (Tk_lit_float $$)       }
  INTCONSTANT         { L _ (Tk_lit_int $$)         }
  BOOLCONSTANT        { L _ (Tk_lit_bool $$)        }
  FIELD_SELECTION     { L _ (Tk_field_selection $$) }
  LEFT_OP             { L _ Tk_p_shiftl             }
  RIGHT_OP            { L _ Tk_p_shiftr             }
   
  INC_OP              { L _ Tk_p_dblplus            }
  DEC_OP              { L _ Tk_p_dbldash            }
  LE_OP               { L _ Tk_p_lesseq             }
  GE_OP               { L _ Tk_p_greatereq          }
  EQ_OP               { L _ Tk_p_equality           }
  NE_OP               { L _ Tk_p_notequal           }
  
  AND_OP              { L _ Tk_p_dblampersand       }
  XOR_OP              { L _ Tk_p_dblcaret           }
  OR_OP               { L _ Tk_p_dblbar             } 
  MUL_ASSIGN          { L _ Tk_p_stareq             }
  DIV_ASSIGN          { L _ Tk_p_divideeq           }
  ADD_ASSIGN          { L _ Tk_p_pluseq             }
  MOD_ASSIGN          { L _ Tk_p_percenteq          }
  LEFT_ASSIGN         { L _ Tk_p_shiftleq           }
  RIGHT_ASSIGN        { L _ Tk_p_shiftreq           }
  AND_ASSIGN          { L _ Tk_p_ampersandeq        }
  XOR_ASSIGN          { L _ Tk_p_careteq            }
  OR_ASSIGN           { L _ Tk_p_bareq              }
  SUB_ASSIGN          { L _ Tk_p_minuseq            }
  
  LEFT_PAREN          { L _ Tk_p_lparen             }
  RIGHT_PAREN         { L _ Tk_p_rparen             }
  LEFT_BRACKET        { L _ Tk_p_lbracket           }
  RIGHT_BRACKET       { L _ Tk_p_rbracket           }   
  LEFT_BRACE          { L _ Tk_p_lbrace             }
  RIGHT_BRACE         { L _ Tk_p_rbrace             }
  DOT                 { L _ Tk_p_dot                }
  
  COMMA               { L _ Tk_p_comma              }
  COLON               { L _ Tk_p_colon              }
  EQUAL               { L _ Tk_p_eq                 }
  SEMICOLON           { L _ Tk_p_semi               }
  BANG                { L _ Tk_p_bang               }
  DASH                { L _ Tk_p_dash               }
  TILDE               { L _ Tk_p_tilde              }
  PLUS                { L _ Tk_p_plus               }
  STAR                { L _ Tk_p_star               }
  SLASH               { L _ Tk_p_divide             }
  PERCENT             { L _ Tk_p_percent            } 

  LEFT_ANGLE          { L _ Tk_p_less               }
  RIGHT_ANGLE         { L _ Tk_p_greater            }
  VERTICAL_BAR        { L _ Tk_p_bar                }  
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

postfix_expression :: { Expr }
  : primary_expression                  { $1 }
  | postfix_expression LEFT_BRACKET integer_expression RIGHT_BRACKET 
                                        { ArrayAccessExpr $1 $3 }
  | function_call                       { $1 }
  | postfix_expression DOT FIELD_SELECTION
                                        { FieldSelectionExpr $1 $3 }
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
                              { (\(n,xs) -> FunCallExpr n (reverse xs)) $1 }
  | function_call_header_no_parameters RIGHT_PAREN          
                              { $1 }
  
function_call_header_no_parameters :: { Expr }
  : function_call_header VOID                       { FunCallExpr $1 [] }
  | function_call_header                            { FunCallExpr $1 [] }
  
function_call_header_with_parameters :: { (Ident, [Expr]) }
  : function_call_header assignment_expression    { ($1,[$2]) } 
  | function_call_header_with_parameters COMMA assignment_expression
                                                  { consSnd $1 $3 }
function_call_header :: { Ident }
  : function_identifier LEFT_PAREN                { $1 }

function_identifier :: { Ident }
  : type_specifier            { constructorIdent $1 }
  | IDENTIFIER                { $1 }
  | FIELD_SELECTION           { $1 }      -- synonymous with Ident

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
  : expression_revlist     { consE $1 }




expression_revlist :: { [Expr] }
  : assignment_expression     { [$1] }
  | expression_revlist COMMA assignment_expression
                              { ($3:$1) }


constant_expression :: { Expr }
  : conditional_expression    { $1 }

declaration :: { Decl }
  : function_prototype SEMICOLON      { FunProtoDecl $1 }
  | init_declarator_list SEMICOLON    { InitDeclr $1 }
  
function_prototype :: { FunProto }
  : function_declarator RIGHT_PAREN   { mkFunProto $1 }
  
function_declarator :: { ((FullType,Ident),[ParamDecl]) }
  : function_header                   { ($1,[]) }
  | function_header_with_parameters   { $1 }

function_header_with_parameters :: { ((FullType,Ident),[ParamDecl]) }
  : function_header parameter_declaration
                                      { ($1,[$2]) }
  | function_header_with_parameters COMMA parameter_declaration
                                      { $1 `consSnd` $3 }

function_header :: { (FullType,Ident) }
  : fully_specified_type IDENTIFIER LEFT_PAREN
                                      { ($1,$2) }

parameter_declarator :: { ParamDeclr }
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


init_declarator_list :: { DeclrList } 
  : init_declarator_revlist   { reverseD $1 }


init_declarator_revlist :: { DeclrRev }
  : single_declaration        { $1 }
  | init_declarator_revlist COMMA IDENTIFIER
                              { $1 `consD` (ScalarDeclr $3 Nothing) }
  | init_declarator_revlist COMMA IDENTIFIER LEFT_BRACKET RIGHT_BRACKET
                              { $1 `consD` (ArrayDeclr $3 Nothing Nothing) }
  | init_declarator_revlist COMMA IDENTIFIER LEFT_BRACKET constant_expression 
                                        RIGHT_BRACKET
                              { $1 `consD` (ArrayDeclr $3 (Just $5) Nothing) }                                        
  | init_declarator_revlist COMMA IDENTIFIER LEFT_BRACKET 
                                        RIGHT_BRACKET EQUAL initializer
                              { $1 `consD` (ArrayDeclr $3 Nothing (Just $7)) }
  | init_declarator_revlist COMMA IDENTIFIER LEFT_BRACKET constant_expression
                                        RIGHT_BRACKET EQUAL initializer
                              { $1 `consD` (ArrayDeclr $3 (Just $5) (Just $8)) }                                        
  | init_declarator_revlist COMMA IDENTIFIER EQUAL initializer
                              { $1 `consD` (ScalarDeclr $3 (Just $5)) }

single_declaration :: { DeclrRev }
  : fully_specified_type      { Declr $1 [] }
  | fully_specified_type IDENTIFIER
                              { Declr $1 [ScalarDeclr $2 Nothing] }
  | fully_specified_type IDENTIFIER LEFT_BRACKET RIGHT_BRACKET
                              { Declr $1 [ArrayDeclr $2 Nothing Nothing] }
  | fully_specified_type IDENTIFIER LEFT_BRACKET constant_expression 
                                        RIGHT_BRACKET
                              { Declr $1 [ArrayDeclr $2 (Just $4) Nothing] }
  | fully_specified_type IDENTIFIER LEFT_BRACKET RIGHT_BRACKET EQUAL initializer
                              { Declr $1 [ArrayDeclr $2 Nothing (Just $6)] }
  | fully_specified_type IDENTIFIER LEFT_BRACKET constant_expression
                                        RIGHT_BRACKET EQUAL initializer
                              { Declr $1 [ArrayDeclr $2 (Just $4) (Just $7)] }
  | fully_specified_type IDENTIFIER EQUAL initializer
                              { Declr $1 [ScalarDeclr $2 (Just $4)]  }
  | INVARIANT IDENTIFIER      { InvariantDeclr $2 [] }
                            
fully_specified_type :: { FullType }
  : type_specifier                      { (Nothing,$1) }
  | type_qualifier type_specifier       { (Just $1,$2) }


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

struct_specifier :: { Struct }
  : STRUCT IDENTIFIER LEFT_BRACE struct_declaration_list RIGHT_BRACE
                              { Struct (Just $2) $4 }
  | STRUCT LEFT_BRACE struct_declaration_list RIGHT_BRACE
                              { Struct Nothing $3 }
                              
struct_declaration_list :: { [StructDeclr] }
  : struct_declaration_revlist          { reverse $1 }
  
struct_declaration_revlist :: { [StructDeclr] }
  : struct_declaration                  { [$1] }
  | struct_declaration_revlist struct_declaration    
                                        { ($2:$1) }

struct_declaration :: { StructDeclr }
  : type_specifier struct_declarator_list SEMICOLON         
                              { StructDeclr $1 $2  }
  
struct_declarator_list :: { [StructDeclrElement] }
  : struct_declarator_revlist { reverse $1 }
  
struct_declarator_revlist :: { [StructDeclrElement] }
  : struct_declarator         { [$1] }
  | struct_declarator_revlist COMMA struct_declarator
                              { ($3:$1) }

struct_declarator :: { StructDeclrElement }
  : IDENTIFIER                { StructScalarDeclr $1 }
  | IDENTIFIER LEFT_BRACKET constant_expression RIGHT_BRACKET
                              { StructArrayDeclr $1 $3 }

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
  | LEFT_BRACE statement_list RIGHT_BRACE         { CompoundStmt $2 }

statement_no_new_scope :: { Stmt }
  : compound_statement_no_new_scope     { $1 }
  | simple_statement                    { $1 }

compound_statement_no_new_scope :: { Stmt }
  : LEFT_BRACE RIGHT_BRACE                        { CompoundStmt [] }
  | LEFT_BRACE statement_list RIGHT_BRACE         { CompoundStmt $2 }

statement_list :: { [Stmt] }
  : statement_revlist         { reverse $1 }
  
statement_revlist :: { [Stmt] }
  : statement                           { [$1] }
  | statement_revlist statement         { ($2:$1) }    -- todo

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
  : translation_unit_revlist            { TranslUnit (reverse $1) }
   
translation_unit_revlist :: { [GblDecl] }
  : external_declaration                                    { [$1] }
  | translation_unit_revlist external_declaration           { ($2 : $1) }
                                          
external_declaration :: { GblDecl }
  : function_definition       { GblFunDef $1 }
  | declaration               { GblDecl $1 }

function_definition :: { FunDef }
  : function_prototype compound_statement_no_new_scope
                              { FunDef $1 $2 }


{

consSnd :: (a, [b]) -> b -> (a, [b])
consSnd (n,xs) x = (n,x:xs)


consE :: [Expr] -> Expr
consE [a] = a
consE xs  = CommaExpr xs


-- just for notational effect
type DeclrRev = DeclrList

consD :: DeclrRev -> DeclrElement -> DeclrRev
consD (Declr ty es)             e = Declr ty (e:es)
consD (InvariantDeclr ident es) e = InvariantDeclr ident (e:es)


reverseD :: DeclrRev -> DeclrList
reverseD (Declr ty es)              = Declr ty (reverse es)
reverseD (InvariantDeclr ident es)  = InvariantDeclr ident (reverse es)


unwrapExpr :: Stmt -> Expr
unwrapExpr (ExprStmt (Just e))  = e
unwrapExpr _                    = error $ "fail"

mkIfStmt :: Expr -> (Stmt, Maybe Stmt) -> Stmt
mkIfStmt condE (thenS,opt_elseS) = IfStmt condE thenS opt_elseS

mkFor :: Expr -> (Expr, Maybe Expr) -> Stmt -> Stmt
mkFor initE (condE,opt_loopE) bodyS = For initE condE opt_loopE bodyS

mkFunProto :: ((FullType,Ident),[ParamDecl]) -> FunProto
mkFunProto ((ty,ident),ps) = FunProto ty ident (reverse ps)



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