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

module CTokens (CToken(..)) where 




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


       

