{-# OPTIONS -fglasgow-exts #-}



-- Generate with 'stackless' 
-- 'compact' doesn't generate type signatures, and without them 
-- GHC spins out of control


module Language.C.Frown.Parser (
  frownParseTranslationUnit
  ) where

import Language.C.Frown.Lexer
import Language.C.Tokens
import Language.C.Syntax
import Language.C.Position

import Prelude hiding (reverse)
import qualified Data.List as List



type Terminal   = CToken


type Result a = Lexer a

frownParseTranslationUnit :: String -> FilePath -> Either String (CTranslationUnit)
frownParseTranslationUnit text filename
  = runLex text filename translation_unit



get = getToken

%{

Terminal        = CTokLParen    as "("
                | CTokRParen    as ")"
                | CTokLBracket  as "["
                | CTokRBracket  as "]"
                | CTokArrow     as "->"
                | CTokDot       as "."
                | CTokExclam    as "!"
                | CTokTilde     as "~"
                | CTokInc       as "++"
                | CTokDec       as "--"
                | CTokPlus      as "+"
                | CTokMinus     as "-"
                | CTokStar      as "*"
                | CTokSlash     as "/"
                | CTokPercent   as "%"
                | CTokAmper     as "&"
                | CTokShiftL    as ">>"
                | CTokShiftR    as "<<"
                | CTokLess      as "<"
                | CTokLessEq    as "<="
                | CTokHigh      as ">"
                | CTokHighEq    as ">="
                | CTokEqual     as "=="
                | CTokUnequal   as "!="
                | CTokHat       as "^"
                | CTokBar       as "|"
                | CTokAnd       as "&&"
                | CTokOr        as "||"
                | CTokQuest     as "?"
                | CTokColon     as ":"
                | CTokAssign    as "="
                | CTokPlusAss   as "+="
                | CTokMinusAss  as "-="
                | CTokStarAss   as "*="
                | CTokSlashAss  as "/="
                | CTokPercAss   as "%="
                | CTokAmpAss    as "&="
                | CTokHatAss    as "^="
                | CTokBarAss    as "|="
                | CTokSLAss     as "<<="
                | CTokSRAss     as ">>="
                | CTokComma     as ","
                | CTokSemic     as ";"
                | CTokLBrace    as "{"
                | CTokRBrace    as "}"
                | CTokEllipsis  as "..."
                | CTokAlignof   as "alignof"
                | CTokAsm       as "asm"
                | CTokAuto      as "auto"
                | CTokBreak     as "break"
                | CTokBool      as "_Bool"
                | CTokCase      as "case"
                | CTokChar      as "char"
                | CTokConst     as "const"
                | CTokContinue  as "continue"
                | CTokComplex   as "_Complex"
                | CTokDefault   as "default"
                | CTokDo        as "do"
                | CTokDouble    as "double"
                | CTokElse      as "else"
                | CTokEnum      as "enum"
                | CTokExtern    as "extern"
                | CTokFloat     as "float"
                | CTokFor       as "for"
                | CTokGoto      as "goto"
                | CTokIf        as "if"
                | CTokInline    as "inline"
                | CTokInt       as "int"
                | CTokLong      as "long"
                | CTokLabel     as "__label__"
                | CTokRegister  as "register"
                | CTokRestrict  as "restrict"
                | CTokReturn    as "return"
                | CTokShort     as "short"
                | CTokSigned    as "signed"
                | CTokSizeof    as "sizeof"
                | CTokStatic    as "static"
                | CTokStruct    as "struct"
                | CTokSwitch    as "switch"
                | CTokTypedef   as "typedef"
                | CTokTypeof    as "typeof"
                | CTokThread    as "__thread"
                | CTokUnion     as "union"
                | CTokUnsigned  as "unsigned"
                | CTokVoid      as "void"
                | CTokVolatile  as "volatile"
                | CTokWhile     as "while"
                | CTokCLit {Char}             as "cchar"          -- character constant
                | CTokILit {Integer}          as "cint"           -- integer constant
                | CTokFLit {String}           as "cfloat"         -- float constant
                | CTokSLit {String}           as "cstr"           -- string constant (no escapes)
                | CTokIdent {String}          as "ident"          -- identifier
                | CTokTyIdent {String}        as "tyident"        -- 'typedef-name' identifier
                | GnuCAttrTok                 as "__attribute__"  -- special GNU C tokens
                | GnuCExtTok                  as "__extension__"  -- special GNU C tokens

                -- special GNU C builtin 'functions' that actually take types as parameters:
                | GnuCVaArg                   as "__builtin_va_arg"
                | GnuCOffsetof                as "__builtin_offsetof"
                | GnuCTyCompat                as "__builtin_types_compatible_p"
                | *CTokEof ;





-- parse a complete C file
--
* translation_unit { CTranslationUnit };
translation_unit 
  {% withLoc (CTranslationUnit (reverse xs))} 
                    : external_declaration_list  {xs} ;


-- parse a complete C translation unit (C99 6.9)
--
-- * GNU extensions:
--     allow empty translation_unit
--     allow redundant ";"
--
external_declaration_list {(Reversed [CExtDecl])};
external_declaration_list 
  {empty}           : ;
  {t}               | external_declaration_list {t}, ";";
  {t `snoc` es}     | external_declaration_list {t}, external_declaration {es} ;


-- parse external C declaration (C99 6.9)
--
-- * GNU extensions:
--     allow extension keyword before external declaration
--     asm definitions
--
external_declaration { CExtDecl };
external_declaration
  {CFDefExt f (reverse ao)}      
                    : attrs_opt {ao}, function_definition {f};
  {CDeclExt d (reverse ao)}      
                    | attrs_opt {ao}, declaration {d}    ;
  {e}               
                    | "__extension__", external_declaration {e}  ;
  {% withLoc (CAsmExt) }       
                    | "asm", "(", string_literal {a}, ")", ";"    ;
  
  

-- parse C function definition (C99 6.9.1)
--
function_definition { CFunDef };
function_definition
  {% withLoc (CFunDef [] fd [] s)}  
                    : function_declarator {fd}, compound_statement {s};

  {% withLoc (CFunDef d fd [] s)} 
                    | declaration_specifier {d}, function_declarator {fd}, compound_statement {s};

  {% withLoc (CFunDef t d [] s)}
                    | type_specifier {t}, function_declarator {d}, compound_statement {s};

  {% withLoc (CFunDef (reverse ds) d [] s)}
                    | declaration_qualifier_list {ds}, function_declarator {d}, compound_statement {s};

  {% withLoc (CFunDef (liftTypeQuals ts) d [] s)}
                    | type_qualifier_list {ts}, function_declarator {d}, compound_statement {s};

  {% withLoc (CFunDef [] d (reverse ds) s)}
                    | old_function_declarator {d}, declaration_list {ds}, compound_statement {s};

  {% withLoc (CFunDef d1 d2 (reverse ds) s)}
                    | declaration_specifier {d1}, old_function_declarator {d2}, declaration_list {ds}, compound_statement {s};

  {% withLoc (CFunDef t d (reverse ds) s)}
                    | type_specifier {t}, old_function_declarator {d}, declaration_list {ds}, compound_statement {s};

  {% withLoc (CFunDef (reverse qs) d (reverse ds) s)}
                    | declaration_qualifier_list {qs}, old_function_declarator {d}, declaration_list {ds}, compound_statement {s};

  {% withLoc (CFunDef (liftTypeQuals qs) d (reverse ds) s)}
                    | type_qualifier_list {qs}, old_function_declarator {d}, declaration_list {ds}, compound_statement {s};
    
    

function_declarator { CDeclr };
function_declarator
  {% do { enterScope
        ; doFuncParamDeclIdent d
        ; return d } }
                    : identifier_declarator {d};


declaration_list {(Reversed [CDecl])};
declaration_list
  {empty}           : ;
  {ds `snoc` d}     | declaration_list {ds}, declaration {d};
  

-- parse C statement (C99 6.8)
--
-- * GNU extension: ' __asm__ (...); ' statements
--
statement { CStat };
statement
  {s}               : labeled_statement {s};
  {s}               | compound_statement {s};
  {s}               | expression_statement {s};
  {s}               | selection_statement {s};
  {s}               | iteration_statement {s};
  {s}               | jump_statement {s};
  {s}               | asm_statement {s};


-- parse C labeled statement (C99 6.8.1)
--
-- * GNU extension: case ranges
--
labeled_statement { CStat };
labeled_statement
  {% withLoc (CLabel n s (reverse ao))}      
                    : identifier {n}, ":", attrs_opt {ao}, statement {s};
  {% withLoc (CCase e s)}
                    | "case", constant_expression {e}, ":", statement {s};
  {% withLoc (CDefault s)}
                    | "default", ":", statement {s};
  {% withLoc (CCases e1 e2 s)}
                    | "case", constant_expression {e1}, "...", constant_expression {e2}, ":", statement {s};
    
    
    

-- parse C compound statement (C99 6.8.2)
--
-- * GNU extension: '__label__ ident;' declarations
--
compound_statement { CStat };
compound_statement
  {% withLoc (CCompound (reverse xs))}
                    : "{", enter_scope{_}, block_item_list {xs}, leave_scope{_}, "}";

  {% withLoc (CCompound (reverse xs))}
                    | "{", enter_scope{_}, label_declarations {ds_}, block_item_list {xs}, leave_scope{_}, "}";


-- No syntax for these, just side effecting semantic actions.
--
--    

enter_scope { () };
enter_scope
  {% enterScope}    : ;
    
leave_scope { () };
leave_scope
  {% leaveScope}    : ;



block_item_list {(Reversed [CBlockItem])};
block_item_list
  {empty}           : ;
  {xs `snoc` x}     | block_item_list {xs}, block_item {x};


block_item { CBlockItem };
block_item
  {CBlockStmt s}    : statement {s};
  {d}               | nested_declaration {d};


nested_declaration { CBlockItem };
nested_declaration
  {CBlockDecl d []}    
                    : declaration {d};
  {CBlockDecl d (reverse as)}    
                    | attrs {as}, declaration {d};
  {CNestedFunDef d []} 
                    | nested_function_definition {d};
  {CNestedFunDef d (reverse as)} 
                    | attrs {as}, nested_function_definition {d};
  {d}               | "__extension__", nested_declaration {d};


nested_function_definition  { CFunDef };
nested_function_definition
  {% withLoc (CFunDef d fd [] s)}
                    : declaration_specifier {d}, function_declarator {fd}, compound_statement {s};

  {% withLoc (CFunDef t fd [] s)}
                    | type_specifier {t}, function_declarator {fd}, compound_statement {s};

  {% withLoc (CFunDef (reverse qs) fd [] s)}
                    | declaration_qualifier_list {qs}, function_declarator {fd}, compound_statement {s};

  {% withLoc (CFunDef (liftTypeQuals qs) fd [] s)}
                    | type_qualifier_list {qs}, function_declarator {fd}, compound_statement {s};


label_declarations { () };
label_declarations
  { () }            : "__label__", identifier_list {ss}, ";";
  { () }            | label_declarations {ls}, "__label__", identifier_list {ss}, ";";
  
  
-- parse C expression statement (C99 6.8.3)
--
expression_statement { CStat };
expression_statement
  {% withLoc (CExpr Nothing)}  
                    : ";" ;
  {% withLoc (CExpr (Just e))}  
                    | expression {e}, ";";


-- parse C selection statement (C99 6.8.4)
--
selection_statement { CStat };
selection_statement
  {% withLoc (CIf e s Nothing)} 
                    : "if", "(", expression {e}, ")", statement {s};

  {% withLoc (CIf e s1 (Just s2))}
                    | "if", "(", expression {e}, ")", statement {s1}, "else", statement {s2};

  {% withLoc (CSwitch e s)}
                    | "switch", "(", expression {e}, ")", statement {s};
  
  
  

-- parse C iteration statement (C99 6.8.5)
--
iteration_statement { CStat };
iteration_statement
  {% withLoc (CWhile e s False)}
                    : "while", "(", expression {e}, ")", statement {s};

  {% withLoc (CWhile e s True)} 
                    | "do", statement {s}, "while", "(", expression {e}, ")", ";";

  {% withLoc (CFor (Left e1) e2 e3 s)}
                    | "for", "(", expression_opt {e1}, ";", expression_opt {e2}, ";", expression_opt {e3}, ")", statement {s};

  {% withLoc (CFor (Right d) e1 e2 s)}
                    | "for", "(", enter_scope{_}, declaration {d}, expression_opt {e1}, ";", expression_opt {e2}, ")", statement {s}, leave_scope{_};
  
  
  
-- parse C jump statement (C99 6.8.6)
--
-- * GNU extension: computed gotos
--
jump_statement { CStat };
jump_statement
  {% withLoc (CGoto s)}
                    : "goto", identifier {s}, ";";
  {% withLoc (CGotoPtr e)}
                    | "goto", "*", expression {e}, ";";
  {% withLoc (CCont)}
                    | "continue", ";";
  {% withLoc (CBreak)}
                    | "break", ";";
  {% withLoc (CReturn e)}
                    | "return", expression_opt {e}, ";";
  
  
  
-- parse GNU C __asm__ (...) statement (recording only a place holder result)
--
asm_statement { CStat };
asm_statement
  {% withLoc (CAsm)} 
                    : "asm", maybe_type_qualifier {qo}, "(", expression {e}, ")", ";";

  {% withLoc (CAsm)}
                    | "asm", maybe_type_qualifier {qo}, "(", expression {e}, ":", asm_operands {asm}, ")", ";";

  {% withLoc (CAsm)}
                    | "asm", maybe_type_qualifier {qo}, "(", expression {e}, ":", asm_operands {asm},
                        ":", asm_operands {asm'}, ")", ";" ;
  {% withLoc (CAsm)}
                    | "asm", maybe_type_qualifier {qo}, "(", expression {e}, ":", asm_operands {asm}, 
                        ":", asm_operands {asm'},
                        ":", asm_clobbers {asm''}, ")", ";" ;




maybe_type_qualifier { () };
maybe_type_qualifier
  { () }            : ;
  { () }            | type_qualifier {q};


asm_operands { () };
asm_operands
  { () }            : ;
  { () }            | nonnull_asm_operands {asm};


nonnull_asm_operands { () };
nonnull_asm_operands
  { () }          : asm_operand {asm};
  { () }          | nonnull_asm_operands {asm}, ",", asm_operand {asm'};


asm_operand { () };
asm_operand
  { () }          : string_literal {s}, "(", expression {e}, ")" ;
  { () }          | "[", "ident" {n}, "]", string_literal {str}, "(", expression {e}, ")";
  { () }          | "[", "tyident" {n}, "]", string_literal {str}, "(", expression {e}, ")";
  
  
  
asm_clobbers { () };
asm_clobbers
  { () }          : string_literal {s};
  { () }          | asm_clobbers {asm}, ",", string_literal {s};


-- parse C declaration (C99 6.7)
--
declaration { CDecl };
declaration
  {% withLoc (CDecl (reverse s) [] ([],[]))}
                  : sue_declaration_specifier {s}, ";";

  {% withLoc (CDecl (reverse s) [] ([],[]))}
                  | sue_type_specifier {s}, ";";

  {case ds of
           CDecl declspecs dies attrs loc ->
                        CDecl declspecs (List.reverse dies) attrs loc}
                  | declaring_list {ds}, ";";

  { case ds of
            CDecl declspecs dies attrs loc ->
              CDecl declspecs (List.reverse dies) attrs loc }
                  | default_declaring_list {ds}, ";";
              
              
              
-- Note that if a typedef were redeclared, then a declaration
-- specifier must be supplied
--
-- Can't redeclare typedef names
--
default_declaring_list { CDecl };
default_declaring_list
  {% let declspecs = reverse qs in
     do { doDeclIdent declspecs d 
        ; loc <- getSrcLoc
        ; return (CDecl (reverse qs) [(Just d, io, Nothing)] ([],reverse as) loc) }}
                    : declaration_qualifier_list {qs}, identifier_declarator {d}, asm_opt {asmo}, attrs_opt {as}, initializer_opt {io};

  
  {% let declspecs = liftTypeQuals qs in
      do { doDeclIdent declspecs d 
         ; loc <- getSrcLoc
         ; return (CDecl (liftTypeQuals qs) [(Just d, io, Nothing)] ([],reverse as) loc) }}
                    | type_qualifier_list {qs}, identifier_declarator {d}, asm_opt {asmo}, attrs_opt {as}, initializer_opt {io};

  {% case ds of
      CDecl declspecs dies attrs loc -> do { doDeclIdent declspecs d
                                           ; return (CDecl declspecs ((Just d, io, Nothing) : dies) (([], reverse as) `pairCat` attrs) loc)} }
               
                    | default_declaring_list {ds}, ",", identifier_declarator {d}, asm_opt {asmo}, attrs_opt {as}, initializer_opt {io};
               
               
               
declaring_list { CDecl };
declaring_list
  {% do { doDeclIdent ds d
        ; loc <- getSrcLoc
        ; return (CDecl ds [(Just d, io, Nothing)] ([],reverse as) loc) }}
                    : declaration_specifier {ds}, declarator {d}, asm_opt {asmo}, attrs_opt {as}, initializer_opt {io};

  {% do { doDeclIdent t d
        ; loc <- getSrcLoc
        ; return (CDecl t [(Just d, io, Nothing)] ([],reverse as) loc) }}
                    | type_specifier {t}, declarator {d}, asm_opt {asmo}, attrs_opt {as}, initializer_opt {io};

  {% case ds of
      CDecl declspecs dies attrs loc -> do { doDeclIdent declspecs d
                                      ; return (CDecl declspecs ((Just d, io, Nothing) : dies) (([],reverse as) `pairCat` attrs) loc)} }
                    | declaring_list {ds}, ",", declarator {d}, asm_opt {asmo}, attrs_opt {as}, initializer_opt {io};
               
               
               
-- parse C declaration specifiers (C99 6.7)
--
-- * summary:
--   [ type_qualifier | storage_class
--   | basic_type_name | elaborated_type_name | tyident ]{
--     (    1 >= basic_type_name
--      |x| 1 == elaborated_type_name
--      |x| 1 == tyident
--     ) && 1 >= storage_class
--   }
--
declaration_specifier { [CDeclSpec] };
declaration_specifier
  {reverse ds}      : basic_declaration_specifier {ds};  -- Arithmetic or void
  {reverse ds}      | sue_declaration_specifier {ds};  -- Struct/Union/Enum
  {reverse ds}      | typedef_declaration_specifier {ds};  -- Typedef
  
  
  

-- A mixture of type qualifiers and storage class specifiers in any order, but
-- containing at least one storage class specifier.
--
-- * summary:
--   [type_qualifier | storage_class]{ 1 >= storage_class }
--
-- * detail:
--   [type_qualifier] storage_class [type_qualifier | storage_class]
--
declaration_qualifier_list {(Reversed [CDeclSpec])};
declaration_qualifier_list
  {singleton (CStorageSpec sc)}
                    : storage_class {sc} ;

  {rmap CTypeQual qs `snoc` CStorageSpec sc}
                    | type_qualifier_list {qs}, storage_class {sc};

  {qs `snoc` q}
                    | declaration_qualifier_list {qs}, declaration_qualifier {q};

  -- (TODO) Unhandled attribute
  {qs}              
                    | declaration_qualifier_list {qs}, attr {z};
    
    
    
declaration_qualifier { CDeclSpec };
declaration_qualifier
  {CStorageSpec sc} : storage_class {sc};
  {CTypeQual q}     | type_qualifier {q};     -- const or volatile


-- parse C storage class specifier (C99 6.7.1)
--
-- * GNU extensions: '__thread' thread local storage
--
storage_class { CStorageSpec };
storage_class
  {% withLoc (CTypedef)}
                    : "typedef";

  {% withLoc (CExtern)}
                    | "extern";
                    
  {% withLoc (CStatic)}
                    | "static";
                    
  {% withLoc (CAuto)}
                    | "auto";
                    
  {% withLoc (CRegister)}
                    | "register";

  {% withLoc (CThread)}
                    | "__thread";





-- parse C type specifier (C99 6.7.2)
--
-- This recignises a whole list of type specifiers rather than just one
-- as in the C99 grammar.
--
-- * summary:
--   [type_qualifier | basic_type_name | elaborated_type_name | tyident]{
--         1 >= basic_type_name
--     |x| 1 == elaborated_type_name
--     |x| 1 == tyident
--   }
--
type_specifier { [CDeclSpec] };
type_specifier
  {reverse ts}      : basic_type_specifier {ts};    -- Arithmetic or void
  {reverse ts}      | sue_type_specifier {ts};      -- Struct/Union/Enum
  {reverse ts}      | typedef_type_specifier {ts};  -- Typedef
  


basic_type_name { CTypeSpec };
basic_type_name
  {% withLoc (CVoidType)}
                    : "void";
                    
  {% withLoc (CCharType)}
                    | "char";
                    
  {% withLoc (CShortType)}
                    | "short";
  
  {% withLoc (CIntType)}
                    | "int";
  
  {% withLoc (CLongType)}
                    | "long";
  
  {% withLoc (CFloatType)}      
                    | "float";
  
  {% withLoc (CDoubleType)}
                    | "double";
  
  {% withLoc (CSignedType)}
                    | "signed";
  
  {% withLoc (CUnsigType)}
                    | "unsigned";
  
  {% withLoc (CBoolType)}
                    | "_Bool";
  
  {% withLoc (CComplexType)}
                    | "_Complex";
  
  
  
  
-- A mixture of type qualifiers, storage class and basic type names in any
-- order, but containing at least one basic type name and at least one storage
-- class specifier.
--
-- * summary:
--   [type_qualifier | storage_class | basic_type_name]{
--     1 >= storage_class && 1 >= basic_type_name
--   }
--
basic_declaration_specifier {(Reversed [CDeclSpec])};
basic_declaration_specifier
  {qs `snoc` (CTypeSpec tn)} 
                    : declaration_qualifier_list {qs}, basic_type_name {tn};

  {ts `snoc` (CStorageSpec sc)} 
                    | basic_type_specifier  {ts}, storage_class {sc};

  {ds `snoc` q}     | basic_declaration_specifier {ds}, declaration_qualifier {q};

  {ds `snoc` (CTypeSpec tn)}
                    | basic_declaration_specifier {ds}, basic_type_name {tn};

  -- TODO: unhandled attribute
  {ds}              
                    | basic_declaration_specifier {ds}, attr {a};





-- A mixture of type qualifiers and basic type names in any order, but
-- containing at least one basic type name.
--
-- * summary:
--   [type_qualifier | basic_type_name]{ 1 >= basic_type_name }
--
basic_type_specifier {(Reversed [CDeclSpec])};
basic_type_specifier
  -- Arithmetic or void
  {singleton (CTypeSpec tn)}
                    : basic_type_name {tn};

  {rmap CTypeQual qs `snoc` CTypeSpec tn}
                    | type_qualifier_list {qs}, basic_type_name {tn};

  {ts `snoc` CTypeQual q}
                    | basic_type_specifier {ts}, type_qualifier {q};

  {ts `snoc` CTypeSpec tn}
                    | basic_type_specifier {ts}, basic_type_name {tn};
  -- TODO: unhandled attribute
  {ts}              
                    | basic_type_specifier {ts}, attr {a};





-- A named or anonymous struct, union or enum type along with at least one
-- storage class and any mix of type qualifiers.
-- 
-- * summary:
--   [type_qualifier | storage_class | elaborated_type_name]{ 
--     1 == elaborated_type_name && 1 >= storage_class
--   }
--
sue_declaration_specifier {(Reversed [CDeclSpec])};
sue_declaration_specifier
  { qs `snoc` CTypeSpec tn }
                  : declaration_qualifier_list {qs}, elaborated_type_name {tn};

  {ts `snoc` CStorageSpec sc}
                  | sue_type_specifier {ts} , storage_class {sc};

  {ts `snoc` q}
                  | sue_declaration_specifier {ts}, declaration_qualifier {q};

  -- TODO: unhandled attribute
  {ts}            
                  | sue_declaration_specifier {ts}, attr {a};




-- A struct, union or enum type (named or anonymous) with optional leading and
-- trailing type qualifiers.
--
-- * summary:
--   [type_qualifier] elaborated_type_name [type_qualifier]
--
sue_type_specifier {(Reversed [CDeclSpec])};
sue_type_specifier
  -- struct/union/enum
  { singleton (CTypeSpec tn) }
                    : elaborated_type_name {tn};

  {rmap CTypeQual qs `snoc` CTypeSpec tn}
                    | type_qualifier_list {qs}, elaborated_type_name {tn};

  {ts `snoc` CTypeQual q}
                    | sue_type_specifier {ts}, type_qualifier {q};
  -- TODO: unhandled attribute
  {ts}              
                    | sue_type_specifier {ts} , attr {a};
    
    
    
    

-- A typedef'ed type identifier with at least one storage qualifier and any
-- number of type qualifiers
--
-- * Summary:
--   [type_qualifier | storage_class | tyident]{
--     1 == tyident && 1 >= storage_class
--   }
--
-- * Note:
--   the tyident can also be a: typeof "(" ... ")"
--
typedef_declaration_specifier {(Reversed [CDeclSpec])};
typedef_declaration_specifier
  {t `snoc` CStorageSpec sc}
                    : typedef_type_specifier {t}, storage_class {sc};

  {% withLoc (\loc -> qs `snoc` (CTypeSpec (CTypeDef t loc)))}
                    | declaration_qualifier_list {qs}, "tyident" {t};

  {% withLoc (\loc -> qs `snoc` (CTypeSpec (CTypeOfExpr e loc)))}
                    | declaration_qualifier_list {qs}, "typeof", "(", expression {e}, ")";

  {% withLoc (\loc -> qs `snoc` (CTypeSpec (CTypeOfType tn loc)))}
                    | declaration_qualifier_list {qs}, "typeof", "(", type_name {tn}, ")";

  {ds `snoc` q}     
                    | typedef_declaration_specifier {ds}, declaration_qualifier {q};

  -- TODO: unhandled attribute
  {d}               
                    | typedef_declaration_specifier {d}, attr {a};
    
    
    

-- typedef'ed type identifier with optional leading and trailing type qualifiers
--
-- * Summary:
--   [type_qualifier] ( tyident | typeof "("...")" ) [type_qualifier]
--
typedef_type_specifier {(Reversed [CDeclSpec])};
typedef_type_specifier
  {% withLoc (\loc -> singleton (CTypeSpec (CTypeDef t loc)))}
                    : "tyident" {t};
    
  {% withLoc (\loc -> singleton (CTypeSpec (CTypeOfExpr e loc)))}
                    | "typeof", "(", expression {e}, ")";

  {% withLoc (\loc -> singleton (CTypeSpec (CTypeOfType tn loc)))}
                    | "typeof", "(", type_name {tn}, ")";

  {% withLoc (\loc -> rmap CTypeQual qs `snoc` (CTypeSpec (CTypeDef t loc)))}
                    | type_qualifier_list {qs}, "tyident" {t};

  {% withLoc (\loc -> rmap CTypeQual qs `snoc` (CTypeSpec (CTypeOfExpr e loc)))}
                    | type_qualifier_list {qs}, "typeof", "(", expression {e}, ")";

  {% withLoc (\loc -> rmap CTypeQual qs `snoc` (CTypeSpec (CTypeOfType tn loc)))}
                    | type_qualifier_list {qs}, "typeof", "(", type_name {tn}, ")";

  {t `snoc` CTypeQual q}
                    | typedef_type_specifier {t}, type_qualifier {q};
  -- TODO: unhandled attibute
  {t}               
                    | typedef_type_specifier {t}, attr {a};



-- A named or anonymous struct, union or enum type.
--
-- * summary:
--   (struct|union|enum) (identifier? "{" ... "}" | identifier)
--
elaborated_type_name { CTypeSpec };
elaborated_type_name
  {% withLoc (CSUType s)}
                    : struct_or_union_specifier {s};
                    
  {% withLoc (CEnumType s)}
                    | enum_specifier {s};
  
  
  
-- parse C structure or union declaration (C99 6.7.2.1)
--
-- * summary:
--   (struct|union) (identifier? "{" ... "}" | identifier)
--
struct_or_union_specifier {CStructUnion};
struct_or_union_specifier
  {% withLoc (CStruct s (Just n) (reverse ds) (reverse ao))}
                    : struct_or_union {s}, attrs_opt {ao}, identifier {n}, "{", struct_declaration_list {ds}, "}";

  {% withLoc (CStruct s Nothing (reverse ds) (reverse ao))}
                    | struct_or_union {s}, attrs_opt {ao}, "{", struct_declaration_list {ds}, "}";

  {% withLoc (CStruct s (Just n) [] (reverse ao))}
                    | struct_or_union {s}, attrs_opt {ao}, identifier {n};


struct_or_union {CStructTag};
struct_or_union
  {CStructTag}      
                    : "struct";
  {CUnionTag}  
                    | "union";


struct_declaration_list {(Reversed [CDecl])};
struct_declaration_list
  {empty}           : ;
  {ds}              | struct_declaration_list {ds}, ";";
  {ds `snoc` d}     | struct_declaration_list {ds}, struct_declaration {d};
  
  
  
  
-- parse C structure declaration (C99 6.7.2.1)
--
struct_declaration { CDecl };
struct_declaration
  {case ds of CDecl declspecs dies attrs loc -> CDecl declspecs (List.reverse dies) attrs loc}
                    : struct_declaring_list {ds}, ";";

  {case ds of CDecl declspecs dies attrs loc -> CDecl declspecs (List.reverse dies) attrs loc}
                    | struct_default_declaring_list {ds}, ";";

  {d}               | "__extension__", struct_declaration {d};


-- doesn't redeclare typedef
struct_default_declaring_list { CDecl };
struct_default_declaring_list
  {% withLoc (\loc -> case d of (d,s) -> CDecl (liftTypeQuals qs) [(d,Nothing,s)] ((reverse ap), (reverse as)) loc)}
                    : attrs_opt {ap}, type_qualifier_list {qs}, struct_identifier_declarator {d}, attrs_opt {as};

  {case ds of
            CDecl declspecs dies attrs loc ->
              case d of
                (d,s) -> CDecl declspecs ((d,Nothing,s) : dies) ((reverse ap, reverse as) `pairCat` attrs) loc}
                    | struct_default_declaring_list {ds}, ",", attrs_opt {ap}, struct_identifier_declarator {d}, attrs_opt {as};
                
                
                

-- * GNU extensions:
--     allow anonymous nested structures and unions
--
struct_declaring_list { CDecl };
struct_declaring_list
  {% withLoc (\loc -> case d of (d,s) -> CDecl t [(d,Nothing,s)] (reverse ap, reverse as) loc)}
                  : attrs_opt {ap}, type_specifier {t}, struct_declarator {d}, attrs_opt {as};

  {case ds of
            CDecl declspecs dies attr loc ->
              case d of
                (d,s) -> CDecl declspecs ((d,Nothing,s) : dies) ((reverse ap,reverse as) `pairCat` attr) loc}
                
                  | struct_declaring_list {ds}, ",", attrs_opt {ap}, struct_declarator {d}, attrs_opt {as} ;

  -- We're being far too liberal in the parsing here, we realyl want to just
  -- allow unnamed struct and union fields but we're actually allowing any
  -- unnamed struct member. Making it allow only unnamed structs or unions in
  -- the parser is far too tricky, it makes things ambiguous. So we'll have to
  -- diagnose unnamed fields that are not structs/unions in a later stage.
  {% withLoc (CDecl t [] (reverse ap,[]))}
                    | attrs_opt {ap}, type_specifier {t};
        
        
        

-- parse C structure declarator (C99 6.7.2.1)
--
struct_declarator  { (Maybe CDeclr, Maybe CExpr) };
struct_declarator
  {(Just d, Nothing)}                    
                    : declarator {d};
  {(Nothing, Just e)}
                    | ":", constant_expression {e};
  {(Just d, Just e)}
                    | declarator {d}, ":", constant_expression {e};


struct_identifier_declarator { (Maybe CDeclr, Maybe CExpr) };
struct_identifier_declarator
  {(Just d, Nothing)}  
                    : identifier_declarator {d};
  {(Nothing, Just e)}
                    | ":", constant_expression {e};
  {(Just d, Just e)}
                    | identifier_declarator {d}, ":", constant_expression {e};
  
  
  

-- parse C enumeration declaration (C99 6.7.2.2)
--
-- * summary:
--   enum (identifier? "{" ... "}" | identifier)
--
enum_specifier { CEnum };
enum_specifier
  {% withLoc (CEnum Nothing (reverse es) (reverse ao))}
                    : "enum", attrs_opt {ao}, "{", enumerator_list {es}, "}";

  {% withLoc (CEnum Nothing (reverse es) (reverse ao))}
                    | "enum", attrs_opt {ao}, "{", enumerator_list {es}, ",", "}";

  {% withLoc (CEnum (Just s) (reverse es) (reverse ao))}
                    | "enum", attrs_opt {ao}, identifier {s}, "{", enumerator_list {es}, "}";

  {% withLoc (CEnum (Just s) (reverse es) (reverse ao))}
                    | "enum", attrs_opt {ao}, identifier {s}, "{", enumerator_list {es}, ",", "}";

  {% withLoc (CEnum (Just s) [] (reverse ao))}
                    | "enum", attrs_opt {ao}, identifier {s};
    
    
    
enumerator_list {(Reversed [(Ident, Maybe CExpr)])};
enumerator_list
  {singleton e}     : enumerator {e};
  {es `snoc` e}     | enumerator_list {es}, ",", enumerator {e};


enumerator { (Ident, Maybe CExpr) };
enumerator
  {(s, Nothing)}    : identifier {s};
  {(s, Just e)}     | identifier {s}, "=", constant_expression {e};


-- parse C type qualifier (C99 6.7.3)
--
type_qualifier { CTypeQual };
type_qualifier
  {% withLoc (CConstQual)}
                    : "const";
                    
  {% withLoc (CVolatQual)}
                    | "volatile";
                    
  {% withLoc (CRestrQual)}
                    | "restrict";
                    
  {% withLoc (CInlinQual)}
                    | "inline";


-- parse C declarator (C99 6.7.5)
--
declarator { CDeclr };
declarator
  {d}               : identifier_declarator {d};
  {d}               | typedef_declarator {d};
  
  
  
-- Parse GNU C's asm annotations
--
asm_opt { () };
asm_opt
  { () }            : ;
  { () }            | "asm", "(", string_literal_list {ss}, ")";


typedef_declarator { CDeclr };
typedef_declarator
  -- would be ambiguous as parameter
  {d}               : paren_typedef_declarator {d};
  
  -- not ambiguous as param
  {d}               | parameter_typedef_declarator {d};


parameter_typedef_declarator { CDeclr };
parameter_typedef_declarator
  {% withLoc (CVarDeclr (Just t))}
                    : "tyident" {t};

  {% withLoc (\loc -> d (CVarDeclr (Just t) loc))}
                    | "tyident" {t}, postfixing_abstract_declarator {d};

  {d}               | clean_typedef_declarator {d};
    
    
    
    

-- The  following have at least one "*".
-- There is no (redundant) "(" between the "*" and the tyident.
clean_typedef_declarator { CDeclr };
clean_typedef_declarator
  {d}               : clean_postfix_typedef_declarator {d};

  {% withLoc (CPtrDeclr [] d [])}
                    | "*", parameter_typedef_declarator {d};
  
  {% withLoc (CPtrDeclr (reverse qs) d [])}
                    | "*", type_qualifier_list {qs}, parameter_typedef_declarator {d};

  {% withLoc (CPtrDeclr [] d (reverse as))}
                    | "*", attrs {as}, parameter_typedef_declarator {d};

  {% withLoc (CPtrDeclr (reverse qs) d (reverse as))}
                    | "*", attrs {as}, type_qualifier_list {qs}, parameter_typedef_declarator {d};

-- TODO: unhandled attributes
clean_postfix_typedef_declarator { CDeclr };
clean_postfix_typedef_declarator
  {d}               : "(", clean_typedef_declarator {d}, ")";
  {d}               | "(", attrs {as}, clean_typedef_declarator {d}, ")";
  {d' d}            | "(", clean_typedef_declarator {d}, ")", postfixing_abstract_declarator {d'};
  {d' d}            | "(", attrs {as}, clean_typedef_declarator {d}, ")", postfixing_abstract_declarator {d'};
  
  

-- The following have a redundant "(" placed
-- immediately to the left of the tyident
paren_typedef_declarator { CDeclr };
paren_typedef_declarator
  {d}               : paren_postfix_typedef_declarator {d};

  -- redundant paren
  {% withLoc (CPtrDeclr [] d [])}
                    | "*", "(", simple_paren_typedef_declarator {d}, ")";

  -- redundant paren
  {% withLoc (CPtrDeclr (reverse qs) d [])}
                    | "*", type_qualifier_list {qs}, "(", simple_paren_typedef_declarator {d}, ")";

  {% withLoc (CPtrDeclr [] d [])}
                    | "*", paren_typedef_declarator {d};

  {% withLoc (CPtrDeclr (reverse qs) d [])}
                    | "*", type_qualifier_list {qs}, paren_typedef_declarator {d};

  {% withLoc (CPtrDeclr [] d (reverse zs))}
                    | "*", attrs {zs}, "(", simple_paren_typedef_declarator {d}, ")";

  -- redundant paren
  {% withLoc (CPtrDeclr (reverse qs) d (reverse zs))}
                    | "*", attrs {zs}, type_qualifier_list {qs}, "(", simple_paren_typedef_declarator {d}, ")";

  {% withLoc (CPtrDeclr [] d (reverse zs))}
                    | "*", attrs {zs}, paren_typedef_declarator {d};

  {% withLoc (CPtrDeclr (reverse qs) d (reverse zs))}
                    | "*", attrs {zs}, type_qualifier_list {qs}, paren_typedef_declarator {d};




-- redundant paren to left of tname
paren_postfix_typedef_declarator { CDeclr };
paren_postfix_typedef_declarator
  {d}               : "(", paren_typedef_declarator {d}, ")";

  -- redundant paren
  {d2 d1}           | "(", simple_paren_typedef_declarator {d1}, postfixing_abstract_declarator {d2}, ")";

  {d2 d1 }          | "(", paren_typedef_declarator {d1}, ")", postfixing_abstract_declarator {d2};
    
    
    
-- Just a type name in any number of nested brackets
--
simple_paren_typedef_declarator { CDeclr };
simple_paren_typedef_declarator
  {% withLoc (CVarDeclr (Just s))}
                    : "tyident" {s};

  {d}               | "(", simple_paren_typedef_declarator {d}, ")";


identifier_declarator { CDeclr };
identifier_declarator
  {d}               : unary_identifier_declarator {d};
  {d}               | paren_identifier_declarator {d};


unary_identifier_declarator { CDeclr };
unary_identifier_declarator
  {d}               : postfix_identifier_declarator {d};

  {% withLoc (CPtrDeclr [] d [])}
                    | "*", identifier_declarator {d};

  {% withLoc (CPtrDeclr (reverse qs) d [])}
                    | "*", type_qualifier_list {qs}, identifier_declarator {d};

  {% withLoc (CPtrDeclr [] d (reverse zs))}
                    | "*", attrs {zs}, identifier_declarator {d};

  {% withLoc (CPtrDeclr (reverse qs) d (reverse zs))}
                    | "*", attrs {zs}, type_qualifier_list {qs}, identifier_declarator {d};
    
    
    
    
postfix_identifier_declarator { CDeclr };
postfix_identifier_declarator
  {d' d}            : paren_identifier_declarator {d}, postfixing_abstract_declarator {d'};

  {d}               | "(", unary_identifier_declarator {d}, ")";

  {d' d}            | "(", unary_identifier_declarator {d}, ")", postfixing_abstract_declarator {d'};

  {d}               | "(", attrs {zs}, unary_identifier_declarator {d}, ")";

  {d' d}            | "(", attrs {zs}, unary_identifier_declarator {d}, ")", postfixing_abstract_declarator {d'};




paren_identifier_declarator { CDeclr };
paren_identifier_declarator
  {% withLoc (CVarDeclr (Just s))} 
                    : "ident" {s};

  {d}               | "(", paren_identifier_declarator {d}, ")";


old_function_declarator { CDeclr };
old_function_declarator
  {d}               : postfix_old_function_declarator {d};

  {% withLoc (CPtrDeclr [] d [])}
                    | "*", old_function_declarator {d};

  {% withLoc (CPtrDeclr (reverse qs) d [])}
                    | "*", type_qualifier_list {qs}, old_function_declarator {d};
    
    

postfix_old_function_declarator { CDeclr };
postfix_old_function_declarator
  {% withLoc (CFunDeclr d [] False) }
                    : paren_identifier_declarator {d}, "(", identifier_list {ns}, ")";

  {d}               | "(", old_function_declarator {d}, ")";

  {d2 d1}            
                    | "(", old_function_declarator {d1}, ")", postfixing_abstract_declarator {d2};


type_qualifier_list {(Reversed [CTypeQual])};
type_qualifier_list
  {singleton qs}    
                    : type_qualifier {qs};
  {qs `snoc` q}     
                    | type_qualifier_list {qs}, type_qualifier {q};
  -- TODO: unhandled attribute                    
  {qs}              
                    | type_qualifier_list {qs}, attr {z};


-- parse C parameter type list (C99 6.7.5)
--
parameter_type_list { ([CDecl], Bool) };
parameter_type_list
  { ([], False)}    : ;
  {(reverse ps, False)} 
                    | parameter_list {ps};
  {(reverse ps, True)}
                    | parameter_list {ps}, ",", "...";

-- TODO: unhandled attributes
parameter_list {(Reversed [CDecl])};
parameter_list
  {singleton d}     
                    : parameter_declaration {d};
  {singleton d}     
                    | attrs {zs}, parameter_declaration {d};
  {ps `snoc` p}     
                    | parameter_list {ps}, ",", attrs_opt {ao}, parameter_declaration {p};
  
  
  


parameter_declaration { CDecl };
parameter_declaration
  {% withLoc (CDecl s [] ([],[]))}
                    : declaration_specifier {s};

  {% withLoc (CDecl s [(Just d, Nothing, Nothing)] ([],[]))}
                    | declaration_specifier {s}, abstract_declarator {d};

  {% withLoc (CDecl s [(Just d, Nothing, Nothing)] ([],reverse as))}
                    | declaration_specifier {s}, identifier_declarator {d}, attrs_opt {as};

  {% withLoc (CDecl s [(Just d, Nothing, Nothing)] ([],reverse as))}
                    | declaration_specifier {s}, parameter_typedef_declarator {d}, attrs_opt {as};

  {% withLoc (CDecl (reverse ds) [] ([],[]))}
                    | declaration_qualifier_list {ds};

  {% withLoc (CDecl (reverse ds) [(Just d, Nothing, Nothing)] ([],[]))}
                    | declaration_qualifier_list {ds}, abstract_declarator {d};

  {% withLoc (CDecl (reverse ds) [(Just d, Nothing, Nothing)] ([],reverse as))}
                    | declaration_qualifier_list {ds}, identifier_declarator {d}, attrs_opt {as};

  {% withLoc (CDecl t [] ([],[]))}
                    | type_specifier {t};

  {% withLoc (CDecl t [(Just d, Nothing, Nothing)] ([],[]))}
                    | type_specifier {t}, abstract_declarator {d};

  {% withLoc (CDecl t [(Just d, Nothing, Nothing)] ([],reverse as))}
                    | type_specifier {t}, identifier_declarator {d}, attrs_opt {as};

  {% withLoc (CDecl t [(Just d, Nothing, Nothing)] ([],reverse as))}
                    | type_specifier {t}, parameter_typedef_declarator {d}, attrs_opt {as};

  {% withLoc (CDecl (liftTypeQuals qs) [] ([],[]))}
                    | type_qualifier_list {qs};

  {% withLoc (CDecl (liftTypeQuals qs) [(Just d, Nothing, Nothing)] ([],[]))}
                    | type_qualifier_list {qs}, abstract_declarator {d};

  {% withLoc (CDecl (liftTypeQuals qs) [(Just d, Nothing, Nothing)] ([],reverse as))}
                    | type_qualifier_list {qs}, identifier_declarator {d}, attrs_opt {as};


identifier_list {(Reversed [Ident])};
identifier_list
  {singleton s}     : "ident" {s};
  {ss `snoc` s}     | identifier_list {ss}, ",", "ident" {s};


-- parse C type name (C99 6.7.6)
--
type_name { CDecl };
type_name
  {% withLoc (CDecl ts [] (reverse ap,[]))}
                    : attrs_opt {ap}, type_specifier {ts};

  {% withLoc (CDecl t [(Just d, Nothing, Nothing)] (reverse ap,[]))}
                    | attrs_opt {ap}, type_specifier {t}, abstract_declarator {d};

  {% withLoc (CDecl (liftTypeQuals qs) [] (reverse ap,[]))}
                    | attrs_opt {ap}, type_qualifier_list {qs};

  {% withLoc (CDecl (liftTypeQuals qs) [(Just d, Nothing, Nothing)] (reverse ap,[]))}
                    | attrs_opt {ap}, type_qualifier_list {qs}, abstract_declarator {d};
    
    
    
-- parse C abstract declarator (C99 6.7.6)
--
abstract_declarator  { CDeclr };
abstract_declarator
  {d}               : unary_abstract_declarator {d};
  {d}               | postfix_abstract_declarator {d};
  -- TODO: unhandled attributes
  {% withLoc (\loc -> d (emptyDeclr loc))}    
                    | postfixing_abstract_declarator {d}, attrs_opt {ao};


postfixing_abstract_declarator { AppCDeclr };
postfixing_abstract_declarator
  {d}               : array_abstract_declarator {d};

  {% withLoc (\loc -> (\declr -> case ps of
                                  (params, variadic) -> CFunDeclr declr params variadic loc)) }             
                    | "(", parameter_type_list {ps}, ")" ;


-- * Note that we recognise but ignore the C99 static keyword (see C99 6.7.5.3)
--
-- * We do not distinguish in the AST between incomplete array types and
-- complete variable length arrays ([ "*" ] means the latter). (see C99 6.7.5.2)
--
array_abstract_declarator { AppCDeclr };
array_abstract_declarator
  {d}               : postfix_array_abstract_declarator {d};

  {\decl -> d2 (d1 decl)}
                    | array_abstract_declarator {d1}, postfix_array_abstract_declarator {d2};
    
    
    
postfix_array_abstract_declarator {AppCDeclr};
postfix_array_abstract_declarator
  {% withLoc (\loc -> (\declr -> CArrDeclr declr [] eo loc))}
                    : "[", assignment_expression_opt {eo}, "]";

  {% withLoc (\loc -> (\declr -> CArrDeclr declr (reverse qs) eo loc))}
                    | "[", type_qualifier_list {qs}, assignment_expression_opt {eo}, "]";

  {% withLoc (\loc -> (\declr -> CArrDeclr declr [] (Just e) loc))}
                    | "[", "static", assignment_expression {e}, "]";

  {% withLoc (\loc -> (\declr -> CArrDeclr declr (reverse qs) (Just e) loc))}
                    | "[", "static", type_qualifier_list {qs}, assignment_expression {e}, "]";

  {% withLoc (\loc -> (\declr -> CArrDeclr declr (reverse qs) (Just e) loc))}
                    | "[", type_qualifier_list {qs}, "static", assignment_expression {e}, "]";

  {% withLoc (\loc -> (\declr -> CArrDeclr declr [] Nothing loc))}
                    | "[", "*", "]";

  {% withLoc (\loc -> (\declr -> CArrDeclr declr (reverse qs) Nothing loc))}
                    | "[", type_qualifier_list {qs}, "*", "]";
    
    
    
unary_abstract_declarator {CDeclr};
unary_abstract_declarator
  {% withLoc (\loc -> CPtrDeclr [] (emptyDeclr loc) [] loc)}
                    : "*";

  {% withLoc (\loc -> CPtrDeclr (reverse qs) (emptyDeclr loc) [] loc)}
                    | "*", type_qualifier_list {qs};

  {% withLoc (CPtrDeclr [] d [])}
                    | "*", abstract_declarator {d};

  {% withLoc (CPtrDeclr (reverse qs) d [])}
                    | "*", type_qualifier_list {qs}, abstract_declarator {d};

  {% withLoc (\loc -> CPtrDeclr [] (emptyDeclr loc) (reverse as) loc )}
                    | "*", attrs {as};

  {% withLoc (\loc -> CPtrDeclr (reverse qs) (emptyDeclr loc) (reverse as) loc)}
                    | "*", attrs {as}, type_qualifier_list {qs};

  {% withLoc (CPtrDeclr [] d (reverse as))}
                    | "*", attrs {as}, abstract_declarator {d};

  {% withLoc (CPtrDeclr (reverse qs) d (reverse as))}
                    | "*", attrs {as}, type_qualifier_list {qs}, abstract_declarator {d};
    
    
-- TODO: unhandled attributes    
postfix_abstract_declarator { CDeclr };
postfix_abstract_declarator
  {d}               : "(", unary_abstract_declarator {d}, ")";
  {d}               | "(", postfix_abstract_declarator {d}, ")";
  
  {% withLoc (\loc -> d (emptyDeclr loc))}
                    | "(", postfixing_abstract_declarator {d}, ")";
  {d2 d1}           | "(", unary_abstract_declarator {d1}, ")", postfixing_abstract_declarator {d2};
  {d}               | "(", attrs {as}, unary_abstract_declarator {d}, ")";
  {d}               | "(", attrs {as}, postfix_abstract_declarator {d}, ")";

  {% withLoc (\loc -> d (emptyDeclr loc))}    
                    | "(", attrs {as}, postfixing_abstract_declarator {d}, ")";
                    
  {d2 d1}           | "(", attrs {as}, unary_abstract_declarator {d1}, ")", postfixing_abstract_declarator {d2};
  {d}               | postfix_abstract_declarator {d}, attr {as};




-- parse C initializer (C99 6.7.8)
--
initializer {CInit};
initializer
  {% withLoc (CInitExpr e)}
                    : assignment_expression {e};
  {% withLoc (CInitList (reverse xs))}
                    | "{", initializer_list {xs}, "}" ;
  {% withLoc (CInitList (reverse xs))}
                    | "{", initializer_list {xs}, ",", "}";


initializer_opt {(Maybe CInit)};
initializer_opt
  {Nothing}         : ;
  {Just x}          | "=", initializer {x};


initializer_list {(Reversed CInitList)};
initializer_list
  {empty}           : ;
  {singleton ([],x)}
                    | initializer {x};
  { singleton (d,x)}
                    | designation {d}, initializer {x};
  {xs `snoc` ([],x)}
                    | initializer_list {xs}, ",", initializer {x};
  {xs `snoc` (d,x)}
                    | initializer_list {xs}, ",", designation {d}, initializer {x};
  
  
-- designation
--
-- * GNU extensions:
--     old style member designation: 'ident :'
--     array range designation
--
designation {[CDesignator]};
designation
  {reverse ds}      : designator_list {ds}, "=";
  {% withLoc (\loc -> [CMemberDesig s loc])}
                    | identifier {s}, ":";
  {[d]}             | array_designator {d};


designator_list {(Reversed [CDesignator])};
designator_list
 {singleton d}      : designator {d};
 {ds `snoc` d}      | designator_list {ds}, designator {d};


designator {CDesignator};
designator
  {% withLoc (CArrDesig e)}
                    : "[", constant_expression {e}, "]";
                    
  {% withLoc (CMemberDesig s)}
                    | ".", identifier {s};
                    
  {d}               | array_designator {d};


array_designator {CDesignator};
array_designator
  {% withLoc (CRangeDesig e1 e2)}
                    : "[", constant_expression {e1}, "...", constant_expression {e2}, "]";
    
    
-- parse C primary expression (C99 6.5.1)
--
-- We cannot use a typedef name as a variable
--
-- * GNU extensions:
--     allow a compound statement as an expression
--     various __builtin_* forms that take type parameters
--
primary_expression {CExpr};
primary_expression
  {% withLoc (CVar s)}          
                    : "ident" {s};
                    
  {% withLoc (CConst c)}        
                    | constant {c};
                    
  {% withLoc (CConst s)}        
                    | string_literal {s};
                    
  {e}               | "(", expression {e}, ")"  ;
  
  {% withLoc (CStatExpr s)}     
                    | "(", compound_statement {s}, ")";

  {% withLoc (CBuiltinExpr)}    
                    | "__builtin_va_arg", "(", assignment_expression {a}, ",", type_name {tn}, ")";

  {% withLoc (CBuiltinExpr)}    
                    | "__builtin_offsetof", "(", type_name {tn}, ",", offsetof_member_designator {d}, ")";

  {% withLoc (CBuiltinExpr)}    
                    | "__builtin_types_compatible_p", "(", type_name {tn1}, ",", type_name {tn2}, ")";


offsetof_member_designator { () };
offsetof_member_designator
  { () }            : "ident" {s};
  { () }            | offsetof_member_designator {d}, ".", "ident" {s};
  { () }            | offsetof_member_designator {d}, "[", expression {e}, "]";
  
  
--parse C postfix expression (C99 6.5.2)
--
postfix_expression {CExpr};
postfix_expression
  {e}               : primary_expression {e};

  {% withLoc (CIndex e1 e2)}    
                    | postfix_expression {e1}, "[", expression {e2}, "]";

  {% withLoc (CCall e [])}
                    | postfix_expression {e}, "(", ")";

  {% withLoc (CCall e (reverse es))}
                    | postfix_expression {e}, "(", argument_expression_list {es}, ")";

  {% withLoc (CMember e s False)}
                    | postfix_expression {e}, ".", identifier {s};

  {% withLoc (CMember e s True)}
                    | postfix_expression {e}, "->", identifier {s};

  {% withLoc (CUnary CPostIncOp e)}
                    | postfix_expression {e}, "++";

  {% withLoc (CUnary CPostDecOp e)}
                    | postfix_expression {e}, "--";

  {% withLoc (CCompoundLit t (reverse xs))}
                    | "(", type_name {t}, ")", "{", initializer_list {xs}, "}";

  {% withLoc (CCompoundLit t (reverse xs))}
                    | "(", type_name {t}, ")", "{", initializer_list {xs}, ",", "}";


argument_expression_list {(Reversed [CExpr])};
argument_expression_list
  {singleton e}     : assignment_expression {e};
  {es `snoc` e}     | argument_expression_list {es}, ",", assignment_expression {e};
  
  
  
-- parse C unary expression (C99 6.5.3)
--
-- * GNU extensions:
--     'alignof' expression or type
--     '__extension__' to suppress warnings about extensions
--     allow taking address of a label with: && label
--
unary_expression { CExpr };
unary_expression
  {e}               : postfix_expression  {e};

  {% withLoc (CUnary CPreIncOp e)}
                    | "++", unary_expression {e};

  {% withLoc (CUnary CPreDecOp e)}
                    | "--", unary_expression {e};

  {e}               | "__extension__", cast_expression {e};

  {% withLoc (CUnary o e)}
                    | unary_operator {o}, cast_expression {e};

  {% withLoc (CSizeofExpr e)} 
                    | "sizeof", unary_expression {e};

  {% withLoc (CSizeofType t)}
                    | "sizeof", "(", type_name {t}, ")";

  {% withLoc (CAlignofExpr e)}
                    | "alignof", unary_expression {e};

  {% withLoc (CAlignofType t)}
                    | "alignof", "(", type_name {t}, ")";
                    
  {% withLoc (CLabAddrExpr s)}
                    | "&&", identifier {s};


unary_operator {CUnaryOp};
unary_operator
  {CAdrOp}          : "&";
  {CIndOp}          | "*";
  {CPlusOp}         | "+";
  {CMinOp}          | "-";
  {CCompOp}         | "~";
  {CNegOp}          | "!";


-- parse C cast expression (C99 6.5.4)
--
cast_expression { CExpr };
cast_expression
  {e}               : unary_expression {e};
  {% withLoc (CCast t e)}
                    | "(", type_name {t}, ")", cast_expression {e};
  
  

-- parse C multiplicative expression (C99 6.5.5)
--
multiplicative_expression { CExpr };
multiplicative_expression
  {e}               : cast_expression {e};

  {% withLoc (CBinary CMulOp e1 e2)} 
                    | multiplicative_expression {e1}, "*", cast_expression {e2};

  {% withLoc (CBinary CDivOp e1 e2)}
                    | multiplicative_expression {e1}, "/", cast_expression {e2};

  {% withLoc (CBinary CRmdOp e1 e2)}
                    | multiplicative_expression {e1}, "%", cast_expression {e2};
    
    
-- parse C additive expression (C99 6.5.6)
--
additive_expression { CExpr };
additive_expression
  {e}               : multiplicative_expression {e};

  {% withLoc (CBinary CAddOp e1 e2)} 
                    | additive_expression {e1}, "+", multiplicative_expression {e2};

  {% withLoc (CBinary CSubOp e1 e2)}
                    | additive_expression {e1}, "-", multiplicative_expression {e2};
    
    
    
-- parse C shift expression (C99 6.5.7)
--
shift_expression { CExpr };
shift_expression
  {e}               : additive_expression {e};

  {% withLoc (CBinary CShlOp e1 e2)}
                    | shift_expression {e1}, "<<", additive_expression {e2};

  {% withLoc (CBinary CShrOp e1 e2)}
                    | shift_expression {e1}, ">>", additive_expression {e2};
    
    
-- parse C relational expression (C99 6.5.8)
--
relational_expression { CExpr };
relational_expression
  {e}               : shift_expression {e};

  {% withLoc (CBinary CLeOp e1 e2)}
                    | relational_expression {e1}, "<", shift_expression {e2};

  {% withLoc (CBinary CGrOp e1 e2)}
                    | relational_expression {e1}, ">", shift_expression {e2};

  {% withLoc (CBinary CLeqOp e1 e2)}
                    | relational_expression {e1}, "<=", shift_expression {e2};

  {% withLoc (CBinary CGeqOp e1 e2)}
                    | relational_expression {e1}, ">=", shift_expression {e2};


-- parse C equality expression (C99 6.5.9)
--
equality_expression { CExpr };
equality_expression
  {e}               : relational_expression {e};

  {% withLoc (CBinary CEqOp  e1 e2)}
                    | equality_expression {e1}, "==", relational_expression {e2};

  {% withLoc (CBinary CNeqOp e1 e2)}
                    | equality_expression {e1}, "!=", relational_expression {e2};
    
    
-- parse C bitwise and expression (C99 6.5.10)
--
and_expression { CExpr };
and_expression
  {e}               : equality_expression {e};

  {% withLoc (CBinary CAndOp e1 e2)}
                    | and_expression{e1}, "&", equality_expression{e2};


-- parse C bitwise exclusive or expression (C99 6.5.11)
--
exclusive_or_expression { CExpr };
exclusive_or_expression
  {e}               : and_expression {e};

  {% withLoc (CBinary CXorOp e1 e2)}
                    | exclusive_or_expression {e1}, "^", and_expression {e2};


-- parse C bitwise or expression (C99 6.5.12)
--
inclusive_or_expression { CExpr };
inclusive_or_expression
  {e}               : exclusive_or_expression {e};

  {% withLoc (CBinary COrOp e1 e2)}
                    | inclusive_or_expression {e1}, "|", exclusive_or_expression {e2};
    
    

-- parse C logical and expression (C99 6.5.13)
--
logical_and_expression { CExpr };
logical_and_expression
  {e}               : inclusive_or_expression {e};

  {% withLoc (CBinary CLndOp e1 e2)}
                    | logical_and_expression {e1}, "&&", inclusive_or_expression {e2};
    
    
-- parse C logical or expression (C99 6.5.14)
--
logical_or_expression { CExpr };
logical_or_expression
  {e}               : logical_and_expression {e};

  {% withLoc (CBinary CLorOp e1 e2)}
                    | logical_or_expression {e1}, "||", logical_and_expression {e2};
    
    
-- parse C conditional expression (C99 6.5.15)
--
-- * GNU extensions:
--     omitting the 'then' part
--
conditional_expression { CExpr };
conditional_expression
  {e}               : logical_or_expression {e};

  {% withLoc (CCond e1 (Just e2) e3)}
                    | logical_or_expression {e1}, "?", expression {e2}, ":", conditional_expression {e3};

  {% withLoc (CCond e1 Nothing e2)}
                    | logical_or_expression {e1}, "?", ":", conditional_expression {e2};
    
    

-- parse C assignment expression (C99 6.5.16)
--
assignment_expression { CExpr };
assignment_expression
  {e}               : conditional_expression {e};

  {% withLoc (CAssign e2 e1 e3)}
                    | unary_expression {e1}, assignment_operator {e2}, assignment_expression {e3};
    
    
assignment_operator {CAssignOp};
assignment_operator
  {CAssignOp}       : "=";
  {CMulAssOp}       | "*=";
  {CDivAssOp}       | "/=";
  {CRmdAssOp}       | "%=";
  {CAddAssOp}       | "+=";
  {CSubAssOp}       | "-=";
  {CShlAssOp}       | "<<=";
  {CShrAssOp}       | ">>=";
  {CAndAssOp}       | "&=";
  {CXorAssOp}       | "^=";
  {COrAssOp}        | "|=";
  
  
  
-- parse C expression (C99 6.5.17)
--
expression { CExpr };
expression
  {e}               : assignment_expression {e};

  {% withLoc (CComma (e1 : reverse e2))}
                    | assignment_expression {e1}, ",", comma_expression {e2};
    
    
comma_expression {(Reversed [CExpr])};
comma_expression
  {singleton e}     : assignment_expression {e};
  {e1 `snoc` e2}    | comma_expression {e1}, ",", assignment_expression {e2};


-- The following was used for clarity
expression_opt {(Maybe CExpr)};
expression_opt
  {Nothing}         : ;
  {Just e}          | expression {e};


-- The following was used for clarity
assignment_expression_opt {(Maybe CExpr)};
assignment_expression_opt
  {Nothing}         : ;
  {Just e}          | assignment_expression {e};
  
  
-- parse C constant expression (C99 6.6)
--
constant_expression {CExpr};
constant_expression
   {e}           : conditional_expression {e};
  
  
-- parse C constants
--
constant {CConst};
constant
  {% withLoc (CIntConst c) }
                  : "cint" {c}  ;
  {% withLoc (CCharConst c) }
                  | "cchar" {c};
  {% withLoc (CFloatConst c) }
                  | "cfloat" {c};
  
  
string_literal {CConst};
string_literal
  {% withLoc (CStrConst s)}
                    : "cstr" {s};
  {% withLoc (CStrConst (concat (s : reverse ss)))}
                    | "cstr" {s}, string_literal_list {ss};



string_literal_list {(Reversed [String])};
string_literal_list
  {singleton s}
                    : "cstr" {s};
  {ss `snoc` s}                    
                    | string_literal_list {ss}, "cstr" {s};
  
  
identifier { Ident };
identifier
  {s}               : "ident" {s};
  {s}               | "tyident" {s};
  
  
-- parse GNU C attribute annotation (junking the result)
--
attrs_opt {(Reversed [CAttributeSpec])};
attrs_opt
  {empty}           
                    : ;
  {ao `snoc` a}            
                    | attrs_opt {ao}, attr {a};


attrs {(Reversed [CAttributeSpec])};
attrs
  {singleton a}            
                    : attr {a};
  {as `snoc` a}            
                    | attrs {as}, attr {a};


attr {CAttributeSpec};
attr
  {% withLoc (CAttributeSpec (reverse xs))}            
                    : "__attribute__", "(", "(", attribute_list {xs}, ")", ")";


attribute_list {(Reversed [CAttribute])};
attribute_list
  {singleton x}            
                    : attribute {x}; 
  {xs `snoc` x}            
                    | attribute_list {xs}, ",", attribute {x}; 
  
  
attribute {CAttribute};
attribute
  {% withLoc (CAttribute "" [])}
                    : ;
  {% withLoc (CAttribute s [])}
                    | "ident" {s};
  {% withLoc (CAttribute "const" [])}
                    | "const";
  {% withLoc (CAttribute s (reverse ps))}            
                    | "ident" {s}, "(", attribute_params {ps}, ")";
  {% withLoc (CAttribute s [])}
                    | "ident" {s}, "(", ")";
  

attribute_params {(Reversed [CExpr])};  
attribute_params
  {singleton e}            
                    : constant_expression {e};
  {es `snoc` e}            
                    | attribute_params {es}, ",", constant_expression {e};

}%




type AppCDeclr = CDeclr -> CDeclr
 

infixr 5 `snoc`

-- Due to the way the grammar is constructed we very often have to build lists
-- in reverse. To make sure we do this consistently and correctly we have a
-- newtype to wrap the reversed style of list:
--
newtype Reversed a = Reversed a

empty :: Reversed [a]
empty = Reversed []

singleton :: a -> Reversed [a]
singleton x = Reversed [x]

snoc :: Reversed [a] -> a -> Reversed [a]
snoc (Reversed xs) x = Reversed (x : xs)

rmap :: (a -> b) -> Reversed [a] -> Reversed [b]
rmap f (Reversed xs) = Reversed (map f xs)

reverse :: Reversed [a] -> [a]
reverse (Reversed xs) = List.reverse xs

liftTypeQuals :: Reversed [CTypeQual] -> [CDeclSpec]
liftTypeQuals (Reversed xs) = revmap [] xs
  where revmap a []     = a
        revmap a (x:xs) = revmap (CTypeQual x : a) xs
        
        
-- We occasionally need things to have a location when they don't naturally
-- have one built in as tokens and most AST elements do.
--
data Located a = Loc !a !Position

unL :: Located a -> a
unL (Loc a pos) = a

instance Pos (Located a) where
  posOf (Loc _ pos) = pos
  


pos :: Position
pos = nopos


emptyDeclr :: SrcLoc -> CDeclr
emptyDeclr pos = CVarDeclr Nothing pos

mkCTokSemic :: CToken
mkCTokSemic = CTokSemic



-- Take the identifiers and use them to update the typedef'ed identifier set
-- if the decl is defining a typedef then we add it to the set,
-- if it's a var decl then that shadows typedefed identifiers
--
doDeclIdent :: [CDeclSpec] -> CDeclr -> Lexer ()
doDeclIdent declspecs declr =
  case getCDeclrIdent declr of
    Nothing -> return ()
    Just ident | any isTypeDef declspecs -> addTypedef ident
               | otherwise               -> shadowTypedef ident

  where isTypeDef (CStorageSpec (CTypedef _)) = True
        isTypeDef _                           = False
        

doFuncParamDeclIdent :: CDeclr -> Lexer ()
doFuncParamDeclIdent (CFunDeclr _ params _ _) =
  sequence_
    [ case getCDeclrIdent declr of
        Nothing -> return ()
        Just ident -> shadowTypedef ident
    | CDecl _ dle _ _ <- params
    , (Just declr, _, _) <- dle ]
    
doFuncParamDeclIdent (CPtrDeclr _ declr _ _) = doFuncParamDeclIdent declr
doFuncParamDeclIdent _ = return ()

        
-- extract all identifiers
getCDeclrIdent :: CDeclr -> Maybe Ident
getCDeclrIdent (CVarDeclr optIde _)         = optIde
getCDeclrIdent (CPtrDeclr _      declr _ _) = getCDeclrIdent declr
getCDeclrIdent (CArrDeclr declr  _     _ _) = getCDeclrIdent declr
getCDeclrIdent (CFunDeclr declr  _     _ _) = getCDeclrIdent declr        

withLoc :: (SrcLoc -> a) -> Lexer a
withLoc fun = do
  (f,l,c) <- getPosition
  return (fun (SrcLoc (Position f l c)))

getSrcLoc :: Lexer SrcLoc
getSrcLoc = do
  (f,l,c) <- getPosition
  return (SrcLoc (Position f l c))


pairCat (a,b) (x,y) = (a++x,b++y)
  