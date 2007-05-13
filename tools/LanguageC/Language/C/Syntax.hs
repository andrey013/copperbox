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
  -- * Source location
  Attrs(..)
  ) where

import Language.C.Position

type Ident = String

-- | A complete C translation unit (K&R A10) (EXPORTED)
--
data CTranslationUnit = CTranslationUnit [CExtDecl]
                       Attrs
             deriving (Eq,Show) 


-- | external C declaration (K&R A10) (EXPORTED)
--
data CExtDecl = CDeclExt  CDecl
              | CFDefExt  CFunDef
              -- | a chunk of assembly code (which is not itself recorded)
              | CAsmExt   Attrs     
              deriving (Eq,Show)     


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
                       Attrs
             deriving (Eq,Show)                        


-- | C statement (A9) (EXPORTED)
--
data CStat 
           -- | label            
           = CLabel     Ident             
                        CStat
                        Attrs             
           -- | constant expression 
           | CCase      CExpr             -- 
                        CStat
                        Attrs
           | CCases     CExpr             -- case range
                        CExpr             -- `case lower .. upper :'
                        CStat
                        Attrs
           -- | default case             
           | CDefault   CStat             
                        Attrs
           | CExpr      (Maybe CExpr)     -- expression statement, maybe empty
                        Attrs
           | CCompound  [CBlockItem]      -- list of declarations and statements
                        Attrs
           -- | conditional expression
           | CIf        CExpr             
                        CStat
                        (Maybe CStat)     -- optional "else" case
                        Attrs
           | CSwitch    CExpr             -- selector
                        CStat
                        Attrs
           | CWhile     CExpr
                        CStat
                        Bool              -- `True' implies "do-while" statement
                        Attrs
           | CFor       (Either (Maybe CExpr)
                                CDecl)
                        (Maybe CExpr)
                        (Maybe CExpr)
                        CStat
                        Attrs
           | CGoto      Ident             -- label
                        Attrs
           | CGotoPtr   CExpr             -- computed address
                        Attrs
           -- | continue statement
           | CCont      Attrs             
           -- | break statement
           | CBreak     Attrs             
           | CReturn    (Maybe CExpr)
                        Attrs
           -- | a chunk of assembly code (which is not itself recorded)          
           | CAsm       Attrs             
           deriving (Eq,Show) 

-- | C99 Block items, things that may appear in compound statements
data CBlockItem = CBlockStmt    CStat
                | CBlockDecl    CDecl
                -- | GNU C has nested functions
                | CNestedFunDef CFunDef               
                deriving (Eq,Show) 



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
                   Attrs
           deriving (Eq,Show)



-- | C declaration specifiers and qualifiers (EXPORTED)
--
data CDeclSpec = CStorageSpec CStorageSpec
               | CTypeSpec    CTypeSpec
               | CTypeQual    CTypeQual
               deriving (Eq,Show)


-- | C storage class specifier (K&R A8.1) (EXPORTED)
--
data CStorageSpec = CAuto     Attrs     
                  | CRegister Attrs 
                  | CStatic   Attrs   
                  | CExtern   Attrs   
                  | CTypedef  Attrs       -- syntactic awkwardness of C
                  | CThread   Attrs       -- GNUC thread local storage
                  deriving (Eq,Show)


-- | C type specifier (K&R A8.2) (EXPORTED)
--
data CTypeSpec = CVoidType    Attrs      
               | CCharType    Attrs      
               | CShortType   Attrs     
               | CIntType     Attrs       
               | CLongType    Attrs      
               | CFloatType   Attrs     
               | CDoubleType  Attrs    
               | CSignedType  Attrs    
               | CUnsigType   Attrs     
               | CBoolType    Attrs      
               | CComplexType Attrs   
               | CSUType      CStructUnion
                              Attrs  
               | CEnumType    CEnum
                              Attrs
               | CTypeDef     Ident                -- typedef name                              
                              Attrs
               | CTypeOfExpr  CExpr
                              Attrs                              
               | CTypeOfType  CDecl
                              Attrs                              
               deriving (Eq,Show)


-- | C type qualifier (K&R A8.2) (EXPORTED)
--
-- * plus \'restrict\' from C99 and \'inline\'
--
data CTypeQual = CConstQual Attrs
               | CVolatQual Attrs
               | CRestrQual Attrs
               | CInlinQual Attrs
               deriving (Eq,Show)


-- | C structure of union declaration (K&R A8.3) (EXPORTED)
--
-- * in both case, either the identifier is present or the list must be
--   non-empty 
--
data CStructUnion = CStruct CStructTag
                            (Maybe Ident)
                            [CDecl]       -- structure declaration
                            Attrs
                  deriving (Eq,Show)



-- (EXPORTED)
--
data CStructTag = CStructTag
                | CUnionTag
                deriving (Eq,Show)

-- | C enumeration declaration (K&R A8.4) (EXPORTED)
--
data CEnum = CEnum (Maybe Ident)
                   [(Ident,                        -- variant name
                     Maybe CExpr)]                -- explicit variant value
                   Attrs
           deriving (Eq,Show)                   



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
                        Attrs
            | CPtrDeclr [[CTypeQual]]                -- indirections (non-empty)
                        CDeclr
                        Attrs                        
            | CArrDeclr CDeclr
                        [CTypeQual]
                        (Maybe CExpr)                -- array size                        
                        Attrs
            | CFunDeclr CDeclr
                        [CDecl]                      --  parameter declarations
                        Bool                        -- is variadic?                        
                        Attrs
            deriving (Eq,Show) 

-- | C initializer (K&R A8.7) (EXPORTED)
--
data CInit = CInitExpr CExpr
                       Attrs                        -- assignment expression
           | CInitList CInitList
                       Attrs
           deriving (Eq,Show)                        

type CInitList = [([CDesignator], CInit)]



-- | C initializer designator (EXPORTED)
--
data CDesignator = CArrDesig     CExpr 
                                 Attrs                                
                 | CMemberDesig  Ident
                                 Attrs                                 
                 | CRangeDesig   CExpr        -- GNUC array range designator
                                 CExpr
                                 Attrs                                
                 deriving (Eq,Show) 


-- | C expression (K&R A7) (EXPORTED)
--
-- * these can be arbitrary expression, as the argument of \'sizeof\' can be
--   arbitrary, even if appearing in a constant expression
--
-- * GNU C extension: \'alignof\'
--
data CExpr = CComma       [CExpr]         -- comma expression list, n >= 2
                          Attrs                        
           | CAssign      CAssignOp       -- assignment operator
                          CExpr           -- l-value
                          CExpr           -- r-value
                          Attrs
           | CCond        CExpr           -- conditional
                   (Maybe CExpr)          -- true-expression (GNU allows omitting)
                          CExpr           -- false-expression
                          Attrs
           | CBinary      CBinaryOp       -- binary operator
                          CExpr           -- lhs
                          CExpr           -- rhs
                          Attrs
           | CCast        CDecl           -- type name
                          CExpr
                          Attrs
           | CUnary       CUnaryOp        -- unary operator
                          CExpr
                          Attrs
           | CSizeofExpr  CExpr
                          Attrs
           | CSizeofType  CDecl           -- type name
                          Attrs
           | CAlignofExpr CExpr
                          Attrs
           | CAlignofType CDecl           -- type name
                          Attrs
           | CIndex       CExpr           -- array
                          CExpr           -- index
                          Attrs
           | CCall        CExpr           -- function
                          [CExpr]         -- arguments
                          Attrs
           | CMember      CExpr           -- structure
                          Ident           -- member name
                          Bool            -- deref structure? (True for `->')
                          Attrs
           | CVar         Ident           -- identifier (incl. enumeration const)
                          Attrs
           | CConst       CConst          -- includes strings
                          Attrs
           | CCompoundLit CDecl           -- C99 compound literal
                          CInitList       -- type name & initialiser list
                          Attrs
           | CStatExpr    CStat           -- GNUC compound statement as expr
                          Attrs
           | CLabAddrExpr Ident           -- GNUC address of label
                          Attrs
           | CBuiltinExpr Attrs           -- place holder for GNUC builtin exprs
           deriving (Eq,Show) 


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
               deriving (Eq,Show)

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
               deriving (Eq,Show)

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
              deriving (Eq,Show)

-- | C constant (K&R A2.5 & A7.2) (EXPORTED)
--
-- * we do not list enumeration constants here, as they are identifiers
--
data CConst = CIntConst   Integer Attrs
            | CCharConst  Char Attrs
            | CFloatConst String Attrs
            | CStrConst   String Attrs
            deriving (Eq,Show) 


-- | For the time being we only use a limited version of Attrs from the 
-- Compiler ToolKit (CTK) that only track source position
data Attrs = OnlyPos Position
  deriving (Eq,Show)


  
              
-- | given an abstract identifier, yield its lexeme (EXPORTED)
--
identToLexeme         :: Ident -> String
identToLexeme s  = s 
              