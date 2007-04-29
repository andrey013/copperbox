
module MakeToken where

import CTokens


makeTerminal :: String ->  CToken
-- keywords
makeTerminal "auto"         = CTokAuto
makeTerminal "break"        = CTokBreak 
{-
makeTerminal "case"         = KW_case 
makeTerminal "char"         = KW_char
makeTerminal "const"        = KW_const 
makeTerminal "continue"     = KW_continue 
makeTerminal "default"      = KW_default 
makeTerminal "do"           = KW_do
makeTerminal "double"       = KW_double 
makeTerminal "else"         = KW_else 
makeTerminal "enum"         = KW_enum 
makeTerminal "extern"       = KW_extern 
makeTerminal "float"        = KW_float 
makeTerminal "for"          = KW_for 
makeTerminal "goto"         = KW_goto 
makeTerminal "if"           = KW_if
makeTerminal "int"          = KW_int 
makeTerminal "long"         = KW_long 
makeTerminal "register"     = KW_register 
makeTerminal "return"       = KW_return 
makeTerminal "short"        = KW_short 
makeTerminal "signed"       = KW_signed 
makeTerminal "sizeof"       = KW_sizeof 
makeTerminal "static"       = KW_static
makeTerminal "struct"       = KW_struct 
makeTerminal "switch"       = KW_switch 
makeTerminal "typedef"      = KW_typedef 
makeTerminal "union"        = KW_union 
makeTerminal "unsigned"     = KW_unsigned 
makeTerminal "void"         = KW_void 
makeTerminal "volatile"     = KW_volatile 
makeTerminal "while"        = KW_while

-- punctuation
makeTerminal "("            = TK_LParen
makeTerminal ")"            = TK_RParen
makeTerminal "{"            = TK_LBrace
makeTerminal "}"            = TK_RBrace
makeTerminal "["            = TK_LBracket
makeTerminal "]"            = TK_RBracket
makeTerminal ","            = TK_Comma
makeTerminal ";"            = TK_Semicolon
makeTerminal ":"            = TK_Colon
makeTerminal "..."          = TK_Ellipsis

  -- equality operators
makeTerminal "=="           = TK_EqualsEquals
makeTerminal "!="           = TK_NotEquals
makeTerminal "<"            = TK_Lt
makeTerminal ">"            = TK_Gt
makeTerminal "<="           = TK_LtEquals
makeTerminal ">="           = TK_GtEquals
  
  
-- unary operators
makeTerminal "&"            = TK_Ampersand
makeTerminal "*"            = TK_Star
makeTerminal "+"            = TK_Plus
makeTerminal "-"            = TK_Minus
makeTerminal "~"            = TK_Tilde
makeTerminal "!"            = TK_Exclaim

-- assignment operators
makeTerminal "="            = TK_Equals
makeTerminal "*="           = TK_MultEquals
makeTerminal "/="           = TK_DivideEquals
makeTerminal "%="           = TK_ModEquals
makeTerminal "+="           = TK_PlusEquals
makeTerminal "-="           = TK_MinusEquals
makeTerminal "<<="          = TK_LeftShiftEquals
makeTerminal ">>="          = TK_RightShiftEquals
makeTerminal "&="           = TK_BitAndEquals
makeTerminal "^="           = TK_BitXorEquals
makeTerminal "|="           = TK_BitOrEquals

-}