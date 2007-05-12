--  C -> Haskell Compiler: Lexer for C Header Files
--
--  Author : Manuel M T Chakravarty, Duncan Coutts
--  Created: 24 May 2005
--
--  Version $Revision: 1.1.2.1 $ from $Date: 2005/06/14 00:16:14 $
--
--  Copyright (c) [1999..2004] Manuel M T Chakravarty
--  Copyright (c) 2005 Duncan Coutts
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
--  C Tokens for the C lexer.
--

module Language.C.Tokens (CToken(..), Repr(..)) where 




-- token definition
-- ----------------

-- possible tokens (EXPORTED)
--
data CToken = CTokLParen          -- `('
	    | CTokRParen              -- `)'
	    | CTokLBracket            -- `['
	    | CTokRBracket            -- `]'
	    | CTokArrow	              -- `->'
	    | CTokDot	              -- `.'
	    | CTokExclam              -- `!'
	    | CTokTilde	              -- `~'
	    | CTokInc	              -- `++'
	    | CTokDec	              -- `--'
	    | CTokPlus	              -- `+'
	    | CTokMinus	              -- `-'
	    | CTokStar	              -- `*'
	    | CTokSlash	              -- `/'
	    | CTokPercent             -- `%'
	    | CTokAmper               -- `&'
	    | CTokShiftL              -- `<<'
	    | CTokShiftR              -- `>>'
	    | CTokLess	              -- `<'
	    | CTokLessEq              -- `<='
	    | CTokHigh	              -- `>'
	    | CTokHighEq              -- `>='
	    | CTokEqual	              -- `=='
	    | CTokUnequal             -- `!='
	    | CTokHat	              -- `^'
	    | CTokBar	              -- `|'
	    | CTokAnd	              -- `&&'
	    | CTokOr	              -- `||'
	    | CTokQuest	              -- `?'
	    | CTokColon	              -- `:'
	    | CTokAssign              -- `='
	    | CTokPlusAss             -- `+='
	    | CTokMinusAss            -- `-='
	    | CTokStarAss             -- `*='
	    | CTokSlashAss            -- `/='
	    | CTokPercAss             -- `%='
	    | CTokAmpAss              -- `&='
	    | CTokHatAss              -- `^='
	    | CTokBarAss              -- `|='
	    | CTokSLAss	              -- `<<='
	    | CTokSRAss	              -- `>>='
	    | CTokComma               -- `,'
	    | CTokSemic               -- `;'
	    | CTokLBrace              -- `{'
	    | CTokRBrace              --
	    | CTokEllipsis            -- `...'
	    | CTokAlignof             -- `alignof' 
						-- (or `__alignof', 
						-- `__alignof__')
	    | CTokAsm                 -- `asm'
	    					-- (or `__asm',
						-- `__asm__')
	    | CTokAuto                -- `auto'
	    | CTokBreak               -- `break'
	    | CTokBool                -- `_Bool'
	    | CTokCase                -- `case'
	    | CTokChar                -- `char'
	    | CTokConst               -- `const' 
						-- (or `__const', `__const__')
	    | CTokContinue            -- `continue' 
	    | CTokComplex             -- `_Complex' 
	    | CTokDefault             -- `default'
	    | CTokDo                  -- `do'
	    | CTokDouble              -- `double'
	    | CTokElse                -- `else'
	    | CTokEnum                -- `enum'
	    | CTokExtern              -- `extern'
 	    | CTokFloat               -- `float'
 	    | CTokFor                 -- `for'
 	    | CTokGoto                -- `goto'
 	    | CTokIf                  -- `if'
      | CTokImaginary           -- `_Imaginary'
	    | CTokInline              -- `inline'
						-- (or `__inline', 
						-- `__inline__')
	    | CTokInt                 -- `int'
	    | CTokLong                -- `long'
	    | CTokLabel               -- `__label__'
	    | CTokRegister            -- `register'
	    | CTokRestrict            -- `restrict'
						-- (or `__restrict', 
						-- `__restrict__')
	    | CTokReturn              -- `return'
	    | CTokShort               -- `short'
	    | CTokSigned              -- `signed'
						-- (or `__signed', 
						-- `__signed__')
	    | CTokSizeof              -- `sizeof'
	    | CTokStatic              -- `static'
	    | CTokStruct              -- `struct'
	    | CTokSwitch              -- `switch'
	    | CTokTypedef             -- `typedef'
	    | CTokTypeof              -- `typeof'
	    | CTokThread              -- `__thread'
	    | CTokUnion               -- `union'
	    | CTokUnsigned            -- `unsigned'
	    | CTokVoid                -- `void'
	    | CTokVolatile            -- `volatile'
						-- (or `__volatile', 
						-- `__volatile__')
	    | CTokWhile               -- `while'
	    | CTokCLit	       !Char	-- character constant
	    | CTokILit	       !Integer	-- integer constant
	    | CTokFLit	       String	-- float constant
	    | CTokSLit	       String	-- string constant (no escapes)
	    | CTokIdent	       String	-- identifier

	      -- not generated here, but in `CParser.parseCHeader'
	    | CTokTyIdent      String	-- `typedef-name' identifier
       -- special GNU C tokens (GnuCTok)
      | GnuCAttrTok             -- `__attribute__'
      | GnuCExtTok              -- `__extension__'
      | GnuCVaArg               -- `__builtin_va_arg'
      | GnuCOffsetof            -- `__builtin_offsetof'
      | GnuCTyCompat            -- `__builtin_types_compatible_p'
	    | CTokEof				          -- end of file
      deriving (Eq,Show)

class Repr a where repr :: a -> String

instance Repr CToken where
  repr CTokLParen               = "("
  repr CTokRParen               = ")"
  repr CTokLBracket             = "["
  repr CTokRBracket             = "]"
  repr CTokArrow                = "->"
  repr CTokDot                  = "."
  repr CTokExclam               = "!"
  repr CTokTilde                = "~"
  repr CTokInc                  = "++"
  repr CTokDec                  = "--"
  repr CTokPlus                 = "+"
  repr CTokMinus                = "-"
  repr CTokStar                 = "*"
  repr CTokSlash                = "/"
  repr CTokPercent              = "%"
  repr CTokAmper                = "&"
  repr CTokShiftL               = "<<"
  repr CTokShiftR               = ">>"
  repr CTokLess                 = "<"
  repr CTokLessEq               = "<="
  repr CTokHigh                 = ">"
  repr CTokHighEq               = ">="
  repr CTokEqual                = "=="
  repr CTokUnequal              = "!="
  repr CTokHat                  = "^"
  repr CTokBar                  = "|"
  repr CTokAnd                  = "&&"
  repr CTokOr                   = "||"
  repr CTokQuest                = "?"
  repr CTokColon                = ":"
  repr CTokAssign               = "="
  repr CTokPlusAss              = "+="
  repr CTokMinusAss             = "-="
  repr CTokStarAss              = "*="
  repr CTokSlashAss             = "/="
  repr CTokPercAss              = "%="
  repr CTokAmpAss               = "&="
  repr CTokHatAss               = "^="
  repr CTokBarAss               = "|="
  repr CTokSLAss                = "<<="
  repr CTokSRAss                = ">>="
  repr CTokComma                = ","
  repr CTokSemic                = ";"
  repr CTokLBrace               = "{"
  repr CTokRBrace               = "}"
  repr CTokEllipsis             = "..."
  repr CTokAlignof              = "alignof"
  repr CTokAsm                  = "asm"
  repr CTokAuto                 = "auto"
  repr CTokBreak                = "break"
  repr CTokBool                 = "_Bool"
  repr CTokCase                 = "case"
  repr CTokChar                 = "char"
  repr CTokConst                = "const" 
  repr CTokContinue             = "continue"
  repr CTokComplex              = "_Complex"
  repr CTokDefault              = "default"
  repr CTokDo                   = "do"
  repr CTokDouble               = "double"
  repr CTokElse                 = "else"
  repr CTokEnum                 = "enum"
  repr CTokExtern               = "extern"
  repr CTokFloat                = "float"
  repr CTokFor                  = "for"
  repr CTokGoto                 = "goto"
  repr CTokIf                   = "if"
  repr CTokImaginary            = "_Imaginary"
  repr CTokInline               = "inline"
  repr CTokInt                  = "int"
  repr CTokLong                 = "long"
  repr CTokLabel                = "__label__"
  repr CTokRegister             = "register"
  repr CTokRestrict             = "restrict"
  repr CTokReturn               = "return"
  repr CTokShort                = "short"
  repr CTokSigned               = "signed"
  repr CTokSizeof               = "sizeof"
  repr CTokStatic               = "static"
  repr CTokStruct               = "struct"
  repr CTokSwitch               = "switch"
  repr CTokTypedef              = "typedef"
  repr CTokTypeof               = "typeof"
  repr CTokThread               = "__thread"
  repr CTokUnion                = "union"
  repr CTokUnsigned             = "unsigned"
  repr CTokVoid                 = "void"
  repr CTokVolatile             = "volatile"
  repr CTokWhile                = "while"
  repr (CTokCLit ch)            = show ch
  repr (CTokILit i)             = show i
  repr (CTokFLit str)           = show str
  repr (CTokSLit str)           = show str
  repr (CTokIdent ident)        = "<identifier>: " ++ ident
  repr (CTokTyIdent ident)      = "<typedef-name>: " ++ ident

  repr GnuCAttrTok              = "__attribute__"
  repr GnuCExtTok               = "__extension__"
  repr GnuCVaArg                = "__builtin_va_arg"
  repr GnuCOffsetof             = "__builtin_offsetof"
  repr GnuCTyCompat             = "__builtin_types_compatible_p"
  repr CTokEof                  = "<eof>"

      
       

