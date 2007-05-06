{-# OPTIONS -fglasgow-exts #-}

--  C->Haskell Compiler: pretty printing of C abstract syntax
--
--  Author : Manuel M T Chakravarty
--  Created: 25 August 1
--
--  Version $Revision: 1.2 $ from $Date: 2004/06/11 07:10:16 $
--
--  Copyright (c) [2001..2004] Manuel M T Chakravarty
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
--  Pretty printing support for abstract C trees.
--
--- DOCU ----------------------------------------------------------------------
--
--  language: Haskell 98
--
--- TODO ----------------------------------------------------------------------
--
--  * So far, only covers a small fraction of the abstract tree definition
--

module CPretty ( Pretty(..)
  -- we are just providing instances to the class `Pretty'
) where

-- import Idents (Ident, identToLexeme)
import Text.PrettyPrint.HughesPJ

import CAST


-- pretty printing of AST nodes
-- ----------------------------
{-
instance Show CDecl where
  showsPrec _ = showString . render . pretty
-}
-- overloaded pretty-printing function (EXPORTED)
--
class Pretty a where
  pretty     :: a -> Doc
  prettyPrec :: Int -> a -> Doc

  pretty       = prettyPrec 0
  prettyPrec _ = pretty


-- actual structure tree traversals
-- --------------------------------

instance Pretty CDecl where
  pretty (CDecl specs declrs ) =
    hsep (map pretty specs) `hang` 2 $
      hsep (punctuate comma (map prettyDeclr declrs)) <> semi

instance Pretty CDeclSpec where
  pretty (CStorageSpec sspec) = pretty sspec
  pretty (CTypeSpec    tspec) = pretty tspec
  pretty (CTypeQual    qspec) = pretty qspec

instance Pretty CStorageSpec where
  pretty (CAuto     ) = text "auto"
  pretty (CRegister ) = text "register"
  pretty (CStatic   ) = text "static"
  pretty (CExtern   ) = text "extern"
  pretty (CTypedef  ) = text "typedef"

instance Pretty CTypeSpec where
  pretty (CVoidType      ) = text "void"
  pretty (CCharType      ) = text "char"
  pretty (CShortType     ) = text "short"
  pretty (CIntType       ) = text "int"
  pretty (CLongType      ) = text "long"
  pretty (CFloatType     ) = text "float"
  pretty (CDoubleType    ) = text "double"
  pretty (CSignedType    ) = text "signed"
  pretty (CUnsigType     ) = text "unsigned"
  pretty (CSUType struct ) = prettySU struct
  pretty (CEnumType enum ) = prettyEnum enum
  pretty (CTypeDef ide   ) = ident ide

instance Pretty CTypeQual where
  pretty (CConstQual ) = text "const"
  pretty (CVolatQual ) = text "volatile"
  pretty (CRestrQual ) = text "restrict"

prettyDeclr :: (Maybe CDeclr, Maybe CInit, Maybe CExpr) -> Doc
prettyDeclr (odeclr, oinit, oexpr) =
      maybe empty pretty odeclr
  <+> maybe empty ((text "=" <+>) . pretty) oinit
  <+> maybe empty ((text ":" <+>) . pretty) oexpr

instance Pretty CDeclr where
  pretty (CVarDeclr oide                   ) = maybe empty ident oide
  pretty (CPtrDeclr inds declr             ) = 
    let
      oneLevel ind = parens . (hsep (map pretty ind) <+>) . (text "*" <>)
    in
    oneLevel inds (pretty declr)
  pretty (CArrDeclr declr _ oexpr          ) =
    pretty declr <> brackets (maybe empty pretty oexpr)
  pretty (CFunDeclr declr decls isVariadic ) =
    let
      varDoc = if isVariadic then text ", ..." else empty
    in
    pretty declr 
    <+> parens (hsep (punctuate comma (map pretty decls)) <> varDoc)






-- auxilliary functions
-- --------------------

ident :: Ident -> Doc
ident  = text . identToLexeme

optName :: (Maybe Ident) -> String
optName = maybe "" $ (++" ").identToLexeme

prettyEnum :: CEnum -> Doc
prettyEnum (CEnum name ms ) = header <> if ms == [] then empty else body ms
       where header = text "enum " <+> maybe empty ident name
             body :: [(Ident, Maybe CExpr)] -> Doc
             body = braces.nest 1.sep.punctuate comma.(map p)
             p :: (Ident, Maybe CExpr) -> Doc
             p (ide, exp) = ident ide <+> maybe empty ((<+> text "=").pretty) exp

prettySU :: CStructUnion -> Doc
prettySU (CStruct t name ms ) = header <> if ms == [] then empty else body ms
    where header = text $ tag t ++ optName name
          tag CStructTag = "struct "
          tag CUnionTag = "union "
          body :: [CDecl] -> Doc
          body = braces.nest 1.sep.map pretty

-- additions (spt)
-- --------------------

instance Pretty CHeader where
  pretty (CHeader extdecls) = vcat (map pretty extdecls)
  
instance Pretty CExtDecl where
  pretty (CDeclExt decl)    = pretty decl
  pretty (CFDefExt fundef)  = pretty fundef
  pretty (CAsmExt _)        = text "<<CPretty: CExtDecl#CAsmExt not yet implemented!>>"
  
instance Pretty [CTypeQual] where
  pretty _ = text "<<CPretty: [CTypeQual] not yet implemented!>>"

instance Pretty CFunDef where
  pretty (CFunDef specs declr decls stat) = 
        hsep (punctuate space (map pretty specs))
    <+> pretty declr  
    <+> hsep (punctuate space (map pretty specs))
    <+> pretty stat
  

instance Pretty CStat where
  pretty (CLabel name stat ) = 
    ident name <> colon <> pretty stat
    
  pretty (CCase expr stat ) = 
    text "case" <> colon <> pretty expr <+> pretty stat
    
  pretty (CDefault stat ) = 
    text "default" <> colon <+> pretty stat
    
  pretty (CExpr oexpr ) = maybe empty pretty oexpr
  
  pretty (CCompound items ) = 
    braces (hsep (map pretty items))
    
  pretty (CIf expr stat ostat ) = 
    text "if" <> parens (pretty expr) <+> pretty stat <+> maybe empty pretty ostat
    
  pretty (CSwitch expr stat ) = 
    text "switch" <> parens (pretty expr) <+> pretty stat 
    
  pretty (CWhile expr stat True ) = 
    text "do" <+> pretty stat <+> text "while" <+> parens (pretty expr) <> semi
  pretty (CWhile expr stat False ) = 
    text "while" <+> parens (pretty expr) <+> pretty stat 
     
  pretty (CFor expr1 expr2 expr3 stat ) = 
    text "for" <+> parens (hsep (punctuate semi (text "<<expr1>>" : map (maybe empty pretty) [expr2, expr3])))

      
  pretty (CGoto name ) = text "goto" <+> ident name <> semi
  pretty (CCont ) = text "continue" <> semi
  pretty (CBreak ) = text "break" <> semi
  pretty (CReturn oexpr ) = text "return" <+> maybe empty pretty oexpr <> semi
  pretty (CAsm ) = text "/* CAsm */"
  
instance Pretty CBlockItem where
  pretty (CBlockStmt    stat)   = pretty stat
  pretty (CBlockDecl    decl)   = pretty decl
  pretty (CNestedFunDef fundef) = pretty fundef


instance Pretty CInit where
  pretty (CInitExpr expr)     = pretty expr
  pretty (CInitList initlist) = hsep (punctuate comma (map pretty initlist))
    
instance Pretty ([CDesignator], CInit) where
  pretty (desigs,cinit) = hsep (map pretty desigs) <+> pretty cinit


instance Pretty CDesignator where 
  pretty (CArrDesig     expr)         = pretty expr                                 
  pretty (CMemberDesig  ident)        = text ident                                 
  pretty (CRangeDesig   expr1 expr2)  = pretty expr1 <+> pretty expr2
                                 

instance Pretty CExpr where
  pretty (CComma exprs)   = hsep (punctuate comma (map pretty exprs))
  pretty (CAssign op lval rval) = pretty lval <+> pretty op <+> pretty rval
  

  
{-  
           | CCond        CExpr           -- conditional
                   (Maybe CExpr)          -- true-expression (GNU allows omitting)
                          CExpr           -- false-expression
           | CBinary      CBinaryOp       -- binary operator
                          CExpr           -- lhs
                          CExpr           -- rhs
           | CCast        CDecl           -- type name
                          CExpr
           | CUnary       CUnaryOp        -- unary operator
                          CExpr
           | CSizeofExpr  CExpr
           | CSizeofType  CDecl           -- type name
           | CAlignofExpr CExpr
           | CAlignofType CDecl           -- type name
-}
  pretty (CIndex arrexpr idxexpr) = pretty arrexpr <> lbrack <> pretty idxexpr <> rbrack                          
  pretty (CCall funexpr args) = pretty funexpr <> parens (hsep (punctuate comma (map pretty args)))

{-                          
           | CMember      CExpr           -- structure
                          Ident           -- member name
                          Bool            -- deref structure? (True for `->')
-}
                          
  pretty (CVar ident)   = text ident
  pretty (CConst cconst) = pretty cconst

{-
           | CCompoundLit CDecl           -- C99 compound literal
                          CInitList       -- type name & initialiser list
           | CStatExpr    CStat           -- GNUC compound statement as expr
           | CLabAddrExpr Ident           -- GNUC address of label


-}
  pretty (CBuiltinExpr) = text "<<CPretty: CExpr#CBuiltinExpr not yet implemented!>>"
  pretty _              = text "<<CPretty: CExpr not yet implemented!>>"
  
  
                                   
instance Pretty CAssignOp where
  pretty CAssignOp    = char '='
  pretty CMulAssOp    = text "*="
  pretty CDivAssOp    = text "/="
  pretty CRmdAssOp    = text "%="
  pretty CAddAssOp    = text "+="
  pretty CSubAssOp    = text "-="
  pretty CShlAssOp    = text "<<="
  pretty CShrAssOp    = text ">>="
  pretty CAndAssOp    = text "&="
  pretty CXorAssOp    = text "^="
  pretty COrAssOp     = text "|="
         
           
instance Pretty CBinaryOp where
  pretty CMulOp     = char '*'
  pretty CDivOp     = char '/'
  pretty CRmdOp     = char '%'
  pretty CAddOp     = char '+'
  pretty CSubOp     = char '-'
  pretty CShlOp     = text "<<"
  pretty CShrOp     = text ">>"
  pretty CLeOp      = char '<'
  pretty CGrOp      = char '>'
  pretty CLeqOp     = text "<="
  pretty CGeqOp     = text ">="
  pretty CEqOp      = text "=="
  pretty CNeqOp     = text "!="
  pretty CAndOp     = char '&'
  pretty CXorOp     = char '^'
  pretty COrOp      = char '|'
  pretty CLndOp     = text "&&"
  pretty CLorOp     = text "||"

instance Pretty CUnaryOp where 
  pretty CPreIncOp    = text "++"
  pretty CPreDecOp    = text "--"
  pretty CPostIncOp   = text "++"
  pretty CPostDecOp   = text "--"
  pretty CAdrOp       = char '&'
  pretty CIndOp       = char '*'
  pretty CPlusOp      = char '+'
  pretty CMinOp       = char '-'
  pretty CCompOp      = char '~'
  pretty CNegOp       = char '!'  


instance Pretty CConst where
  pretty (CIntConst   i )   = integer i 
  pretty (CCharConst  c )   = quotes (char c)
  pretty (CFloatConst s )   = text s
  pretty (CStrConst   s )   = doubleQuotes (text s)
    
             