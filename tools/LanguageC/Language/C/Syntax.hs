{-# OPTIONS_GHC -fglasgow-exts #-}
--------------------------------------------------------------------------------
--  C -> Haskell Compiler: Abstract Syntax for Source Files
--
--  Author : Manuel M T Chakravarty
--  Created: 7 March 99
--
--  Version $Revision: 1.10 $ from $Date: 2004/06/11 07:10:16 $
--
--  Copyright (c) [1999..2004] Manuel M T Chakravarty
--
--  This file is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation; either version 2 of the License, or
--  (at your option) any later version.
--
--  This file is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--- DESCRIPTION ---------------------------------------------------------------
--
--  Abstract syntax of C files.
--
--- DOCU ----------------------------------------------------------------------
--
--  language: Haskell 98
--
--  The tree structure corresponds to the grammar in Appendix A of K&R.  This
--  abstract syntax simplifies the concrete syntax by merging similar concrete
--  constructs into a single type of abstract tree structure: declarations are
--  merged with structure declarations, parameter declarations and type names,
--  and declarators are merged with abstract declarators.
--
--  With K&R we refer to ``The C Programming Language'', second edition, Brain
--  W. Kernighan and Dennis M. Ritchie, Prentice Hall, 1988.  This module
--  supports the C99 `restrict' extension
--  <http://www.lysator.liu.se/c/restrict.html>, `inline' functions, and also
--  the GNU C `alignof' extension.
--
--- TODO ----------------------------------------------------------------------
--

module Language.C.Syntax (
  -- * External Declarations
  CTranslationUnit(..), CExtDecl(..), CFunDef(..),
  -- * Statements
  CStat(..), CBlockItem(..),
  -- * Declarations
  CDecl(..), CDeclSpec(..), CStorageSpec(..), CTypeSpec(..),
  CTypeQual(..), CStructUnion(..),  CStructTag(..), CEnum(..),
  CDeclr(..), CInit(..), CInitList, CDesignator(..), 
  -- * Expressions             
  CExpr(..), CAssignOp(..), CBinaryOp(..), CUnaryOp(..), CConst (..), 
  -- * Identifiers
  Ident, identToLexeme,
  -- * GCC Attributes
  CAttributeSpec(..), CAttribute(..), 
  -- * Source location
  SrcLoc(..)
  ) where

import Language.C.Position

import Data.Generics.Basics
import Data.Generics.Instances

type Ident = String

-- | A complete C translation unit (K&R A10) (EXPORTED)
--
data CTranslationUnit = CTranslationUnit [CExtDecl]
                       SrcLoc
             deriving (Eq,Ord,Show,Typeable,Data) 


-- | external C declaration (K&R A10) (EXPORTED)
--
data CExtDecl = CDeclExt  CDecl
                          [CAttributeSpec]
              | CFDefExt  CFunDef 
                          [CAttributeSpec]
              -- | a chunk of assembly code (which is not itself recorded)
              | CAsmExt   SrcLoc     
              deriving (Eq,Ord,Show,Typeable,Data)     


-- | C function definition (K&R A10.1) (EXPORTED)
--
-- * The only type specifiers allowed are \'extern\' and \'static\'.
--
-- * The declarator must specify explicitly that the declared identifier has
--   function type.
--
-- * The optional declaration list is for old\-style function declarations.
--
-- * The statement must be a compound statement.
--
data CFunDef = CFunDef [CDeclSpec]        -- type specifier and qualifier
                       CDeclr             -- declarator
                       [CDecl]            -- optional declaration list
                       CStat              -- compound statement
                       SrcLoc
             deriving (Eq,Ord,Show,Typeable,Data)                        


-- | C statement (A9) (EXPORTED)
--
data CStat 
           -- | label            
           = CLabel     Ident             
                        CStat
                        [CAttributeSpec]
                        SrcLoc             
           -- | constant expression 
           | CCase      CExpr             -- 
                        CStat
                        SrcLoc
           | CCases     CExpr             -- case range
                        CExpr             -- `case lower .. upper :'
                        CStat
                        SrcLoc
           -- | default case             
           | CDefault   CStat             
                        SrcLoc
           | CExpr      (Maybe CExpr)     -- expression statement, maybe empty
                        SrcLoc
           | CCompound  [CBlockItem]      -- list of declarations and statements
                        SrcLoc
           -- | conditional expression
           | CIf        CExpr             
                        CStat
                        (Maybe CStat)     -- optional "else" case
                        SrcLoc
           | CSwitch    CExpr             -- selector
                        CStat
                        SrcLoc
           | CWhile     CExpr
                        CStat
                        Bool              -- `True' implies "do-while" statement
                        SrcLoc
           | CFor       (Either (Maybe CExpr)
                                CDecl)
                        (Maybe CExpr)
                        (Maybe CExpr)
                        CStat
                        SrcLoc
           | CGoto      Ident             -- label
                        SrcLoc
           | CGotoPtr   CExpr             -- computed address
                        SrcLoc
           -- | continue statement
           | CCont      SrcLoc             
           -- | break statement
           | CBreak     SrcLoc             
           | CReturn    (Maybe CExpr)
                        SrcLoc
           -- | a chunk of assembly code (which is not itself recorded)          
           | CAsm       SrcLoc             
           deriving (Eq,Ord,Show,Typeable,Data) 

-- | C99 Block items, things that may appear in compound statements
data CBlockItem = CBlockStmt    CStat
                | CBlockDecl    CDecl
                                [CAttributeSpec]
                -- | GNU C has nested functions
                | CNestedFunDef CFunDef
                                [CAttributeSpec]               
                deriving (Eq,Ord,Show,Typeable,Data) 



-- | C declaration (K&R A8), structure declaration (K&R A8.3), parameter
-- declaration (K&R A8.6.3), and type name (K&R A8.8) (EXPORTED) 
--
-- * Toplevel declarations (K&R A8): 
--
--   - they require that the type specifier and qualifier list is not empty,
--     but gcc allows it and just issues a warning; for the time being, we
--     also allow it;
--   - at most one storage class specifier is allowed per declaration;
--   - declarators must be present and size expressions are not allowed, ie,
--     the elements of K&R's init-declarator-list are represented by triples
--     of the form \'(Just declr, oinit, Nothing)\', where \'oinit\' maybe
--     Nothing\' or \'Just init\'; and
--   - abstract declarators are not allowed.
--
-- * Structure declarations (K&R A8.3):
--
--   - do not allow storage specifiers;
--   - do not allow initializers; 
--   - require a non-empty declarator-triple list, where abstract declarators 
--     are not allowed; and
--   - each of the declarator-triples has to contain either a declarator or a
--     size expression, or both, ie, it has the form `(Just decl, Nothing,
--     Nothing)', `(Nothing, Nothing, Just size)', or `(Just decl, Nothing,
--     Just size)'.
--
-- * Parameter declarations (K&R A8.6.3):
--
--   - allow neither initializers nor size expressions;
--   - allow at most one declarator triple of the form `(Just declr, Nothing, 
--     Nothing)' (in case of an empty declarator, the list must be empty); and
--   - allow abstract declarators.
--
-- * Type names (A8.8):
--
--   - do not allow storage specifiers;
--   - allow neither initializers nor size expressions; and
--   - allow at most one declarator triple of the form `(Just declr, Nothing, 
--     Nothing)' (in case of an empty declarator, the list must be empty),
--     where the declarator must be abstract, ie, must not contain a declared
--     identifier. 
--
data CDecl = CDecl [CDeclSpec]            -- type specifier and qualifier
                   [(Maybe CDeclr,        -- declarator (may be omitted)
                     Maybe CInit,         -- optional initializer
                     Maybe CExpr)]        -- optional size (const expr)
                   ([CAttributeSpec],[CAttributeSpec]) -- a attributes might prefix or suffix the decl
                   SrcLoc
           deriving (Eq,Ord,Show,Typeable,Data)



-- | C declaration specifiers and qualifiers (EXPORTED)
--
data CDeclSpec = CStorageSpec CStorageSpec
               | CTypeSpec    CTypeSpec
               | CTypeQual    CTypeQual
               deriving (Eq,Ord,Show,Typeable,Data)


-- | C storage class specifier (K&R A8.1) (EXPORTED)
--
data CStorageSpec = CAuto     SrcLoc     
                  | CRegister SrcLoc 
                  | CStatic   SrcLoc   
                  | CExtern   SrcLoc   
                  | CTypedef  SrcLoc       -- syntactic awkwardness of C
                  | CThread   SrcLoc       -- GNUC thread local storage
                  deriving (Eq,Ord,Show,Typeable,Data)


-- | C type specifier (K&R A8.2) (EXPORTED)
--
data CTypeSpec = CVoidType    SrcLoc      
               | CCharType    SrcLoc      
               | CShortType   SrcLoc     
               | CIntType     SrcLoc       
               | CLongType    SrcLoc      
               | CFloatType   SrcLoc     
               | CDoubleType  SrcLoc    
               | CSignedType  SrcLoc    
               | CUnsigType   SrcLoc     
               | CBoolType    SrcLoc      
               | CComplexType SrcLoc   
               | CSUType      CStructUnion
                              SrcLoc  
               | CEnumType    CEnum
                              SrcLoc
               | CTypeDef     Ident                -- typedef name                              
                              SrcLoc
               | CTypeOfExpr  CExpr
                              SrcLoc                              
               | CTypeOfType  CDecl
                              SrcLoc                              
               deriving (Eq,Ord,Show,Typeable,Data)


-- | C type qualifier (K&R A8.2) (EXPORTED)
--
-- * plus \'restrict\' from C99 and \'inline\'
--
data CTypeQual = CConstQual SrcLoc
               | CVolatQual SrcLoc
               | CRestrQual SrcLoc
               | CInlinQual SrcLoc
               deriving (Eq,Ord,Show,Typeable,Data)


-- | C structure of union declaration (K&R A8.3) (EXPORTED)
--
-- * in both case, either the identifier is present or the list must be
--   non-empty 
--
data CStructUnion = CStruct CStructTag
                            (Maybe Ident)
                            [CDecl]       -- structure declaration
                            [CAttributeSpec]
                            SrcLoc
                  deriving (Eq,Ord,Show,Typeable,Data)



-- (EXPORTED)
--
data CStructTag = CStructTag
                | CUnionTag
                deriving (Eq,Ord,Show,Typeable,Data)

-- | C enumeration declaration (K&R A8.4) (EXPORTED)
--
data CEnum = CEnum (Maybe Ident)
                   [(Ident,                        -- variant name
                     Maybe CExpr)]                -- explicit variant value
                   [CAttributeSpec]
                   SrcLoc
           deriving (Eq,Ord,Show,Typeable,Data)                   



-- | C declarator (K&R A8.5) and abstract declarator (K&R A8.8) (EXPORTED)
--
-- * We have one type qualifer list `[CTypeQual]' for each indirection (ie,
--   each occurrence of \'*\' in the concrete syntax).
--
-- * We unfold K&R's direct-declarators nonterminal into declarators.  Note
--   that \'*(*x)\' is equivalent to \'**x\'.
--
-- * Declarators (A8.5) and abstract declarators (A8.8) are represented in the 
--   same structure.  In the case of a declarator, the identifier in
--   `CVarDeclr' must be present; in an abstract declarator it misses.
--   \'CVarDeclr Nothing ...' on its own is meaningless, it may only occur as
--   part of a larger type (ie, there must be a pointer, an array, or function
--   declarator around).
--
-- * The qualifiers list in a `CPtrDeclr' may not be empty.
--
-- * Old and new style function definitions are merged into a single case
--   `CFunDeclr'.  In case of an old style definition, the parameter list is
--   empty and the variadic flag is \'False\' (ie, the parameter names are not
--   stored in the tree).  Remember, a new style definition with no parameters 
--   requires a single \'void\' in the argument list (according to the standard).
--
-- * We unfold K&R's parameter-type-list nonterminal into the declarator
--   variant for functions.
--
data CDeclr = CVarDeclr (Maybe Ident)                -- declared identifier                        
                        SrcLoc
            | CPtrDeclr [CTypeQual]                -- indirections (non-empty)
                        CDeclr
                        [CAttributeSpec]
                        SrcLoc                        
            | CArrDeclr CDeclr
                        [CTypeQual]
                        (Maybe CExpr)                -- array size                        
                        SrcLoc
            | CFunDeclr CDeclr
                        [CDecl]                      --  parameter declarations
                        Bool                        -- is variadic?                        
                        SrcLoc
            deriving (Eq,Ord,Show,Typeable,Data) 

-- | C initializer (K&R A8.7) (EXPORTED)
--
data CInit = CInitExpr CExpr
                       SrcLoc                        -- assignment expression
           | CInitList CInitList
                       SrcLoc
           deriving (Eq,Ord,Show,Typeable,Data)                        

type CInitList = [([CDesignator], CInit)]



-- | C initializer designator (EXPORTED)
--
data CDesignator = CArrDesig     CExpr 
                                 SrcLoc                                
                 | CMemberDesig  Ident
                                 SrcLoc                                 
                 | CRangeDesig   CExpr        -- GNUC array range designator
                                 CExpr
                                 SrcLoc                                
                 deriving (Eq,Ord,Show,Typeable,Data) 


-- | C expression (K&R A7) (EXPORTED)
--
-- * these can be arbitrary expression, as the argument of \'sizeof\' can be
--   arbitrary, even if appearing in a constant expression
--
-- * GNU C extension: \'alignof\'
--
data CExpr = CComma       [CExpr]         -- comma expression list, n >= 2
                          SrcLoc                        
           | CAssign      CAssignOp       -- assignment operator
                          CExpr           -- l-value
                          CExpr           -- r-value
                          SrcLoc
           | CCond        CExpr           -- conditional
                   (Maybe CExpr)          -- true-expression (GNU allows omitting)
                          CExpr           -- false-expression
                          SrcLoc
           | CBinary      CBinaryOp       -- binary operator
                          CExpr           -- lhs
                          CExpr           -- rhs
                          SrcLoc
           | CCast        CDecl           -- type name
                          CExpr
                          SrcLoc
           | CUnary       CUnaryOp        -- unary operator
                          CExpr
                          SrcLoc
           | CSizeofExpr  CExpr
                          SrcLoc
           | CSizeofType  CDecl           -- type name
                          SrcLoc
           | CAlignofExpr CExpr
                          SrcLoc
           | CAlignofType CDecl           -- type name
                          SrcLoc
           | CIndex       CExpr           -- array
                          CExpr           -- index
                          SrcLoc
           | CCall        CExpr           -- function
                          [CExpr]         -- arguments
                          SrcLoc
           | CMember      CExpr           -- structure
                          Ident           -- member name
                          Bool            -- deref structure? (True for `->')
                          SrcLoc
           | CVar         Ident           -- identifier (incl. enumeration const)
                          SrcLoc
           | CConst       CConst          -- includes strings
                          SrcLoc
           | CCompoundLit CDecl           -- C99 compound literal
                          CInitList       -- type name & initialiser list
                          SrcLoc
           | CStatExpr    CStat           -- GNUC compound statement as expr
                          SrcLoc
           | CLabAddrExpr Ident           -- GNUC address of label
                          SrcLoc
           | CBuiltinExpr SrcLoc           -- place holder for GNUC builtin exprs
           deriving (Eq,Ord,Show,Typeable,Data) 


-- | C assignment operators (K&R A7.17) (EXPORTED)
--
data CAssignOp = CAssignOp
               | CMulAssOp
               | CDivAssOp
               | CRmdAssOp                -- remainder and assignment
               | CAddAssOp
               | CSubAssOp
               | CShlAssOp
               | CShrAssOp
               | CAndAssOp
               | CXorAssOp
               | COrAssOp
               deriving (Eq,Ord,Show,Typeable,Data)

-- | C binary operators (K&R A7.6-15) (EXPORTED)
--
data CBinaryOp = CMulOp
               | CDivOp
               | CRmdOp                   -- remainder of division
               | CAddOp
               | CSubOp
               | CShlOp                   -- shift left
               | CShrOp                   -- shift right
               | CLeOp                    -- less
               | CGrOp                    -- greater
               | CLeqOp                   -- less or equal
               | CGeqOp                   -- greater or equal
               | CEqOp                    -- equal
               | CNeqOp                   -- not equal
               | CAndOp                   -- bitwise and
               | CXorOp                   -- exclusive bitwise or
               | COrOp                    -- inclusive bitwise or
               | CLndOp                   -- logical and
               | CLorOp                   -- logical or
               deriving (Eq,Ord,Show,Typeable,Data)

-- | C unary operator (K&R A7.3-4) (EXPORTED)
--
data CUnaryOp = CPreIncOp                 -- prefix increment operator
              | CPreDecOp                 -- prefix decrement operator
              | CPostIncOp                -- postfix increment operator
              | CPostDecOp                -- postfix decrement operator
              | CAdrOp                    -- address operator
              | CIndOp                    -- indirection operator
              | CPlusOp                   -- prefix plus
              | CMinOp                    -- prefix minus
              | CCompOp                   -- one's complement
              | CNegOp                    -- logical negation
              deriving (Eq,Ord,Show,Typeable,Data)

-- | C constant (K&R A2.5 & A7.2) (EXPORTED)
--
-- * we do not list enumeration constants here, as they are identifiers
--
data CConst = CIntConst   Integer SrcLoc
            | CCharConst  Char SrcLoc
            | CFloatConst String SrcLoc
            | CStrConst   String SrcLoc
            deriving (Eq,Ord,Show,Typeable,Data) 


-- | GCC attribute specifier
data CAttributeSpec = CAttributeSpec [CAttribute]
                                     SrcLoc
                    deriving (Eq,Ord,Show,Typeable,Data)

-- | GCC attribute
data CAttribute = CAttribute Ident
                             [CExpr]
                             SrcLoc
                  deriving (Eq,Ord,Show,Typeable,Data)
                             


-- | Renamed Attrs from C2Hs to SrcLoc - this is to be distinct 
-- from GCC attributes. Also, SrcLoc is more limited than C2Hs
-- attrs.
data SrcLoc = SrcLoc Position
  deriving (Eq,Ord,Show,Typeable,Data)


  
              
-- | given an abstract identifier, yield its lexeme (EXPORTED)
--
identToLexeme         :: Ident -> String
identToLexeme s  = s 
              