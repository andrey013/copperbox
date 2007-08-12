{-# OPTIONS_GHC -fglasgow-exts #-}

module Language.C.Pretty where

import Language.C.Syntax
import Language.C.Pretty.CompactPretty

import Control.Applicative
import Control.Monad.Reader


import Data.Maybe




data PPStyle = PPStyle {
  indent_size :: Int,
  indentBlock :: PM Doc -> PM Doc -> PM Doc
  }

plainStyle = PPStyle { 
  indent_size = 4,
  indentBlock = braceAllman
  }
  
  
type PM = Reader PPStyle


instance Applicative PM where
  pure = return 
  (<*>) = ap


  
  
data Assoc = LR | RL | NA
  deriving (Eq,Show)

data Fixity = Prefix | Postfix | Infix Assoc | Primary
  deriving (Eq,Show)

type Precedence = Int 

type Operator = (String,Precedence, Fixity)




maxprec = 16
maxrator = ("<max-precedence sentinel>", maxprec, Infix NA)


stringrep  (a,_,_) = a
precedence (_,b,_) = b
fixity     (_,_,c) = c


class Rator a where
  rator :: a -> Operator

instance Rator CUnaryOp where 
  rator CPostIncOp   = ("++", 16, Postfix)
  rator CPostDecOp   = ("--", 16, Postfix)
  rator CPreIncOp    = ("++", 15, Prefix)
  rator CPreDecOp    = ("--", 15, Prefix)
  rator CAdrOp       = ("&",  15, Prefix)
  rator CIndOp       = ("*",  15, Prefix)
  rator CPlusOp      = ("+",  15, Prefix)
  rator CMinOp       = ("-",  15, Prefix)
  rator CCompOp      = ("~",  15, Prefix)
  rator CNegOp       = ("!",  15, Prefix)  
  
instance Rator CBinaryOp where
  rator CMulOp     = ("*",  13, Infix LR)
  rator CDivOp     = ("/",  13, Infix LR)
  rator CRmdOp     = ("%",  13, Infix LR)
  rator CAddOp     = ("+",  12, Infix LR)
  rator CSubOp     = ("-",  12, Infix LR)
  rator CShlOp     = ("<<", 11, Infix LR)
  rator CShrOp     = (">>", 11, Infix LR)
  rator CLeOp      = ("<",  10, Infix LR)
  rator CGrOp      = (">",  10, Infix LR)
  rator CLeqOp     = ("<=", 10, Infix LR)
  rator CGeqOp     = (">=", 10, Infix LR)
  rator CEqOp      = ("==",  9, Infix LR)
  rator CNeqOp     = ("!=",  9, Infix LR)
  rator CAndOp     = ("&",   8, Infix LR)
  rator CXorOp     = ("^",   7, Infix LR)
  rator COrOp      = ("|",   6, Infix LR)
  rator CLndOp     = ("&&",  5, Infix LR)
  rator CLorOp     = ("||",  4, Infix LR)
  
instance Rator CAssignOp where
  rator CAssignOp    = ("=",   2, Infix RL)
  rator CMulAssOp    = ("*=",  2, Infix RL)
  rator CDivAssOp    = ("/=",  2, Infix RL)
  rator CRmdAssOp    = ("%=",  2, Infix RL)
  rator CAddAssOp    = ("+=",  2, Infix RL)
  rator CSubAssOp    = ("-=",  2, Infix RL)
  rator CShlAssOp    = ("<<=", 2, Infix RL)
  rator CShrAssOp    = (">>=", 2, Infix RL)
  rator CAndAssOp    = ("&=",  2, Infix RL)
  rator CXorAssOp    = ("^=",  2, Infix RL)
  rator COrAssOp     = ("|=",  2, Infix RL)
  
type Fragment = (Operator, PM Doc)  


runPretty :: PM Doc -> PPStyle -> Doc
runPretty d style = runReader d style 

bracket :: Fragment -> Assoc -> Operator -> PM Doc
bracket (inner,doc) side outer 
    | noparens inner outer side == True = doc
    | otherwise                         = parens doc

                              
noparens :: Operator -> Operator -> Assoc -> Bool              
noparens (_,pi,fi) (_,po, fo) side
  | pi > po   = True
  | otherwise = 
    case (fi, side) of
      (Primary,   _) -> True
      (Postfix,  LR) -> True
      (Prefix,   RL) -> True
      (Infix LR, LR) -> pi == po && fo == Infix LR
      (Infix RL, RL) -> pi == po && fo == Infix RL
      (_,        NA) -> fi == fo
      _              -> False


attribSep, attribSepE, enumSep, enumSepE, structSep, structSepE, tupleSep, tupleSepE 
            :: Applicative f => [f Doc] -> f Doc
        
attribSep   = encloseSep lparen rparen comma
attribSepE  = encloseSepE lparen rparen comma

enumSep     = encloseSep lbrace rbrace comma 
enumSepE    = encloseSepE lbrace rbrace comma 

structSep   = encloseSep lbrace rbrace semi 
structSepE  = encloseSepE lbrace rbrace semi 

tupleSep    = encloseSep lparen rparen comma
tupleSepE   = encloseSepE lparen rparen comma


-- | subscript - array subscripting
subscript e e' = e >~< char '[' >~< e' >~< char ']'

--------------------------------------------------------------------------------
-- brace styles
--------------------------------------------------------------------------------

levelSize :: PM Int
levelSize = asks indent_size

braceAllman :: PM Doc -> PM Doc -> PM Doc
braceAllman pre inner = levelSize >>= f
  where f i = pre ^+^ lbrace ^+^ (nest i inner) ^+^ rbrace


  

instance Pretty PM CTranslationUnit where
  pp (CTranslationUnit ext_decls _) = linesep 2 (docs ext_decls)

        
instance Pretty PM CExtDecl where
  pp (CDeclExt decl _)    = pp decl >~< semi
  pp (CFDefExt fun_def _) = pp fun_def
  pp (CAsmExt _)          = text "/* ASM */"
        
instance Pretty PM CFunDef where
  pp (CFunDef specs declr decls stmt _) =
    (suffixes space (docs specs))         -- style poor here 
      >~< (pp declr)
      >~< (prefixes space (docs decls))
      ^+^ pp stmt

                                          
instance Pretty PM CStat where
  pp (CLabel ident stmt attrs _) = 
    text ident >~< colon >~< pp stmt
   
  pp (CCase expr stmt _) = 
    text "case" >+< pp expr >~< colon >+<  pp stmt
          
  pp (CCases lexpr uexpr stmt _) = 
    text "case" >+< pp lexpr >+< text "..." >+< pp uexpr 
                >~< colon >+< pp stmt
    
  pp (CDefault stmt _) = text "default" >~< colon >+< pp stmt
    
  pp (CExpr oexpr _) = ppo oexpr >~< semi
  
  pp (CCompound blockitems _) = 
    encloseSep lbrace rbrace line (docs blockitems)
    
  pp (CIf condexpr thenstat Nothing _) = asks indentBlock >>= \f ->
    f (text "if" >~< parens (pp condexpr)) (pp thenstat)

  pp (CIf condexpr tstmt (Just estmt) _) = asks indentBlock >>= \f ->
    f (text "if" >~< parens (pp condexpr)) (pp tstmt)
      ^+^ f (text "else")  (pp estmt)
    
  pp (CSwitch expr stmt _) =  text "switch" >~< parens (pp expr) >+< pp stmt
  
  pp (CWhile expr stmt True _) = 
    text "do" >+< pp stmt >+< text "while" >+< parens (pp expr) >~< semi
    
  pp (CWhile expr stmt False _) =     
    text "while" >+< parens (pp expr) >+< pp stmt
     
  pp (CFor initial otestexpr oupdexpr stmt _) = 
    text "for" >~< encloseSep lparen rparen semi clauses
               ^+^ pp stmt
    where clauses = [ppInitialClause initial, f otestexpr, f oupdexpr]
          f = maybe space pp 
          
  pp (CGoto ident _) = text "goto" >+< text ident >~< semi
 
  pp (CCont _) = text "continue" >~< semi

  pp (CBreak _) = text "break" >~< semi

  pp (CReturn oexpr _) = text "return" >+< ppo oexpr >~< semi

  pp (CAsm _) = text "/* CAsm */"


instance Pretty PM CBlockItem where
  pp (CBlockStmt stmt)            = pp stmt
  pp (CBlockDecl decl attrs)      = pp decl >~< semi
  pp (CNestedFunDef fundef attrs) = pp fundef

  
instance Pretty PM CDecl where
  pp (CDecl specs params _ _) = 
    spaceSep (docs specs) >+< commaSep (map ppDeclParam params)
    
                             
instance Pretty PM CDeclSpec where
  pp (CStorageSpec spec) = pp spec
  pp (CTypeSpec spec)    = pp spec
  pp (CTypeQual qual)    = pp qual


instance Pretty PM CStorageSpec where
  pp (CAuto     _)  = text "auto"
  pp (CRegister _)  = text "register"
  pp (CStatic   _)  = text "static"
  pp (CExtern   _)  = text "extern"
  pp (CTypedef  _)  = text "typedef"
  pp (CThread   _)  = text "thread"

instance Pretty PM CTypeSpec where
  pp (CVoidType _)        = text "void"
  pp (CCharType _)        = text "char"
  pp (CShortType _)       = text "short"
  pp (CIntType _)         = text "int"
  pp (CLongType _)        = text "long"
  pp (CFloatType _)       = text "float"
  pp (CDoubleType _)      = text "double"
  pp (CSignedType _)      = text "signed"
  pp (CUnsigType _)       = text "unsigned"
  pp (CBoolType _)        = text "_Bool"
  pp (CComplexType _)     = text "_Complex"
  pp (CSUType su _)       = pp su
  pp (CEnumType enum _)   = text "enum" >+< pp enum
  pp (CTypeDef name _)    = text name
  pp (CTypeOfExpr expr _) = text "typeof" >~< parens (pp expr)
  pp (CTypeOfType decl _) = text "typeof" >~< parens (pp decl)
                    
instance Pretty PM CTypeQual where
  pp (CConstQual _) = text "const"
  pp (CVolatQual _) = text "volatile"
  pp (CRestrQual _) = text "restrict"
  pp (CInlinQual _) = text "inline"
                                 

instance Pretty PM CStructUnion where
  pp (CStruct tag oident decls attrs _) = 
    pp tag >+< spaceSep (docs attrs) >+< ppOptIdent oident 
           >+< structSepE (docs decls)

instance Pretty PM CStructTag where 
  pp CStructTag   = text "struct"
  pp CUnionTag    = text "union"
                                  

instance Pretty PM CEnum where
  pp (CEnum oident elts attrs _) = 
    spaceSep (docs attrs) >+< ppOptIdent oident >+< enumSep (map ppEnumElt elts)

                                            
instance Pretty PM CDeclr where
  pp (CVarDeclr oi _) = ppOptIdent oi
  
  pp (CPtrDeclr quals declr _ _) = 
    char '*' >~< spaceSep (docs quals) >+< pp declr
  
  pp (CArrDeclr declr [] oexpr _) = 
    pp declr >~< brackets (ppo oexpr)

  pp (CArrDeclr declr quals oexpr _) = 
    pp declr >~< brackets (spaceSep (map pp quals) >+< ppo oexpr)   

  pp (CFunDeclr declr decls True _) =
    pp declr >~< tupleSep (docs decls) >~< text ", ..."
    
  pp (CFunDeclr declr decls False _) =
    pp declr >~< tupleSep (docs decls) 

instance Pretty PM CInit where
  pp (CInitExpr expr _) = pp expr
  pp (CInitList inits _) = commaSep (map ppInit inits)

  


instance Pretty PM CDesignator where
  pp (CArrDesig expr _) = brackets (pp expr)
  pp (CMemberDesig ident _) = dot >~< text ident
  pp (CRangeDesig lexpr rexpr _) = 
    brackets $ pp lexpr >~< text "..." >~< pp rexpr


instance Rator CExpr where
  rator (CComma _ _)          = ("<comma>", 1, Infix LR) 
  rator (CCond _ _ _ _)       = ("<ternary>", 3, Primary)                 
  rator (CBinary op _ _ _)    = rator op
  rator (CCast _ _ _)         = ("<cast>", 3, Infix RL)
  rator (CUnary op _ _)       = rator op 
  rator (CSizeofExpr _ _)     = ("<sizeof_expr>", 15, Infix RL)
  rator (CSizeofType _ _)     = ("<sizeof_type>", 15, Infix RL)
  rator (CAlignofExpr _ _)    = ("<alignof_expr>", 15, Infix RL)
  rator (CAlignofType _ _)    = ("<alignof_type>", 15, Infix RL)
  rator (CIndex _ _ _)        = ("<subscript>", 16, Postfix)
  rator (CCall _ _ _)         = ("<function_call>", 16, Postfix)
  rator (CMember _ _ _ _)     = ("<selection>", 16, Postfix)
  rator (CVar _ _)            = ("<variable>", 16, Primary)
  rator (CConst _ _)          = ("<constant>", 16, Primary)
  rator (CCompoundLit _ _ _)  = ("<comp_lit>", 16, Postfix)
  rator (CStatExpr _ _)       = ("<stat_expr>", 15, Infix RL)
  rator (CLabAddrExpr _ _)    = ("<addr_expr>", 15, Prefix)
  rator (CBuiltinExpr _)      = ("<builtin>", 15, Prefix)




instance Pretty PM CExpr where
  pp (CComma exprs _) = commaSep (docs exprs)
  
  pp (CAssign op lexpr rexpr _) = 
    pp lexpr >+< text (stringrep $ rator op) >+< pp rexpr
   
  pp (CCond cexpr otexpr fexpr _) = 
    pp cexpr >+< char '?' >+< ppo otexpr >+< char ':' >+< pp fexpr
        
                                  
  pp e@(CBinary op lexpr rexpr _) = left_e >+< text (stringrep rtr) >+< right_e
    where rtr = rator e
          left_e = bracket (rator lexpr, pp lexpr) LR rtr
          right_e = bracket (rator rexpr, pp rexpr) RL rtr
        


  pp (CCast decl expr _) = parens (pp decl) >+< pp expr
        

  pp e@(CUnary op expr _) = 
    case (fixity rtr) of 
      Postfix -> expr'  >~< text (stringrep rtr)
      Prefix -> text (stringrep rtr) >~< expr'
      _ -> error "Unary expression not deemed to be pre/post-fix"
    where rtr = rator e
          expr' = bracket (rator expr, pp expr) NA rtr
                                   
  pp (CSizeofExpr expr _) = text "sizeof" >+< parens (pp expr)
            
  pp (CSizeofType decl _) = text "sizeof" >+< parens (pp decl)
        
  pp (CAlignofExpr expr _) = text "alignof" >+< parens (pp expr)
    
  pp (CAlignofType decl _) = text "alignof" >+< parens (pp decl)
    
  pp (CIndex arrexpr idxexpr _) = subscript (pp arrexpr) (pp idxexpr)
        
  pp (CCall expr exprs _) = pp expr >~< tupleSep (docs exprs)
                
  pp (CMember expr ident True _) = pp expr >~< text "->" >~< text ident
  
  pp (CMember expr ident False _) = pp expr >~< char '.' >~< text ident
                           
  pp (CVar ident _) = text ident
        
  pp (CConst cst _) = pp cst

  pp (CCompoundLit decl inits _) =  
    parens (pp decl) >+< encloseSep lbrace rbrace space (map ppInit inits)
        
  pp (CStatExpr stmt _) = parens (pp stmt)
        
  pp (CLabAddrExpr ident _) = text "&&" >~< text ident
                
  pp (CBuiltinExpr _) = text "<<CPretty: CExpr#CBuiltinExpr not yet implemented!>>"
       
                                       
instance Pretty PM CAssignOp where
  pp CAssignOp    = char '='
  pp CMulAssOp    = text "*="
  pp CDivAssOp    = text "/="
  pp CRmdAssOp    = text "%="
  pp CAddAssOp    = text "+="
  pp CSubAssOp    = text "-="
  pp CShlAssOp    = text "<<="
  pp CShrAssOp    = text ">>="
  pp CAndAssOp    = text "&="
  pp CXorAssOp    = text "^="
  pp COrAssOp     = text "|="
         
           
instance Pretty PM CBinaryOp where
  pp CMulOp     = char '*'
  pp CDivOp     = char '/'
  pp CRmdOp     = char '%'
  pp CAddOp     = char '+'
  pp CSubOp     = char '-'
  pp CShlOp     = text "<<"
  pp CShrOp     = text ">>"
  pp CLeOp      = char '<'
  pp CGrOp      = char '>'
  pp CLeqOp     = text "<="
  pp CGeqOp     = text ">="
  pp CEqOp      = text "=="
  pp CNeqOp     = text "!="
  pp CAndOp     = char '&'
  pp CXorOp     = char '^'
  pp COrOp      = char '|'
  pp CLndOp     = text "&&"
  pp CLorOp     = text "||"

instance Pretty PM CUnaryOp where 
  pp CPreIncOp    = text "++"
  pp CPreDecOp    = text "--"
  pp CPostIncOp   = text "++"
  pp CPostDecOp   = text "--"
  pp CAdrOp       = char '&'
  pp CIndOp       = char '*'
  pp CPlusOp      = char '+'
  pp CMinOp       = char '-'
  pp CCompOp      = char '~'
  pp CNegOp       = char '!'  
  
instance Pretty PM CConst where
  pp (CIntConst   i _)   = integer i 
  pp (CCharConst  c _)   = squotes (char c)
  pp (CFloatConst s _)   = text s
  pp (CStrConst   s _)   = dquotes (text s)
    

instance Pretty PM CAttributeSpec where
  pp (CAttributeSpec attrs _) =  
    text "__attribute__" >~< dparens (spaceSep $ docs attrs)


instance Pretty PM CAttribute where
  pp (CAttribute "" [] _) = nulldoc
  
  pp (CAttribute s [] _)  = text s
  
  pp (CAttribute s ps _)  = f 
    where f = text s >~< parens (spaceSep $ docs ps)

                 

ppDeclParam :: (Maybe CDeclr, Maybe CInit, Maybe CExpr) -> PM Doc
ppDeclParam (odeclr, oinit, oexpr) = ppo odeclr >+< ppo oinit >+< ppo oexpr

ppOptIdent :: (Maybe Ident) -> PM Doc
ppOptIdent = maybe nulldoc text 

ppInitialClause :: Either (Maybe CExpr) CDecl -> PM Doc
ppInitialClause (Left Nothing)     = space
ppInitialClause (Left (Just expr)) = pp expr
ppInitialClause (Right decl)       = pp decl

ppEnumElt :: (Ident, Maybe CExpr) -> PM Doc
ppEnumElt (ident, Nothing) = text ident
ppEnumElt (ident, Just e)  = text ident >~< equals >~< pp e


ppInit :: ([CDesignator], CInit) -> PM Doc
ppInit (desigs, cinit) = spaceSep (docs desigs) >+< pp cinit


