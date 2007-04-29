-- NOTE
-- Alex is order sensitive

{

module CLexerAlt  where

import CTokens
import MakeToken



}

%wrapper "frown"

$nondigit           = [A-Za-z_]
$digit              = [0-9]
$nonzero_digit      = [1-9]
$octal_digit        = [0-7]
$hex_digit          = [0-9A-Fa-f]

$long_suffix        = [lL]
$unsigned_suffix    = [uU]
@long_long_suffix   = u{2} | U{2}

@suffix_ul          = $unsigned_suffix ($long_suffix? | @long_long_suffix?)
@suffix_lu          = ($long_suffix | @long_long_suffix) $unsigned_suffix?
@integer_suffix     = @suffix_ul | @suffix_lu 


@digit_sequence     = $digit+
@hex_digit_sequence = $hex_digit+
$sign_part          = [\-\+]
$floating_suffix    = [fFlL]

@hex_prefix         = 0x | 0X


@exponent           = (e|E) $sign_part? @digit_sequence 
@binary_exponent    = (p|P) $sign_part? @digit_sequence 


@dotted_digits      = @digit_sequence \. 
                    | @digit_sequence \. @digit_sequence 
                    | \. @digit_sequence
                                        
@dotted_hex_digits  = @hex_digit_sequence \. 
                    | @hex_digit_sequence \. @hex_digit_sequence 
                    | \. @hex_digit_sequence

$identifier_nondigit  = $nondigit
$identifier           = [$nondigit $digit]


-- to test... [ \' \" \? \\ \a \b \f \r \t \v ]
$character_escape_code = [n t b r f v \\ \' \" a \?]
@octal_escape_code    = $octal_digit{1,3}
@hex_escape_code      = x $hex_digit+

@escape_code          = $character_escape_code 
                      | @octal_escape_code
                      | $character_escape_code
                      
@escape_character     = \\ @escape_code 

@c_char               = [\x00-\xff] # [\' \n \\]
                      | @escape_character 

@c_char_sequence      = @c_char+


@s_char               = [\x00-\xff] # [\" \n \\]
                      | @escape_character     

@s_char_sequence      = @s_char+


tokens :-


-- decimal floating-point constants
<0> { 
  @digit_sequence @exponent $floating_suffix?   { error "decimal floating constant" }
  
  @dotted_digits @exponent? $floating_suffix?   { error "decimal floating constant" } 
}

-- hexadecimal floating-point constants
<0> {
  @hex_prefix @dotted_hex_digits @binary_exponent $floating_suffix? { error "hexadecimal floating constant" } 
  
  @hex_prefix @hex_digit_sequence @binary_exponent $floating_suffix? { error "hexadecimal floating constant" } 
}

-- punctuators
<0> {
  
  \!              { punctuator CTokExclam }
  \%              { punctuator CTokPercent }
  \^              { punctuator CTokHat }
  \&              { punctuator CTokAmper }
  \*              { punctuator CTokStar }
  \-              { punctuator CTokMinus }
  \+              { punctuator CTokPlus }
  \=              { punctuator CTokAssign }
  \~              { punctuator CTokTilde }
  \|              { punctuator CTokBar }
  \.              { punctuator CTokDot } 
  \<              { punctuator CTokLess }
  \>              { punctuator CTokHigh }    
  \/              { punctuator CTokSlash }
  \?              { punctuator CTokQuest }
  
  "+="            { punctuator CTokPlusAss }
  "-="            { punctuator CTokMinusAss }
  "*="            { punctuator CTokStarAss }
  "/="            { punctuator CTokSlashAss }
  "%="            { punctuator CTokPercAss }
  "<<="           { punctuator CTokSLAss }
  ">>="           { punctuator CTokSRAss }
  "&="            { punctuator CTokAmpAss }
  "^="            { punctuator CTokHatAss }
  "|="            { punctuator CTokBarAss }

  "->"            { punctuator CTokArrow }
  "++"            { punctuator CTokInc }
  "--"            { punctuator CTokDec }           
  "<<"            { punctuator CTokShiftL }
  ">>"            { punctuator CTokShiftR }  
  "<="            { punctuator CTokLessEq }
  ">="            { punctuator CTokHighEq }
  "=="            { punctuator CTokEqual }
  "!="            { punctuator CTokUnequal }
  "&&"            { punctuator CTokAnd }
  "||"            { punctuator CTokOr }
    
  \(              { punctuator CTokLParen }
  \)              { punctuator CTokRParen }
  \[              { punctuator CTokLBracket }
  \]              { punctuator CTokRBracket }
  \{              { punctuator CTokLBrace }
  \}              { punctuator CTokRBrace }
  \,              { punctuator CTokComma }
  \;              { punctuator CTokSemic }
  \:              { punctuator CTokColon }  
  "..."           { punctuator CTokEllipsis }


--  "<%"            { punctuator undefined }
--  "%>"            { punctuator undefined }  
--  "<:"            { punctuator undefined }
--  ":>"            { punctuator undefined }
--  "%:"            { punctuator undefined }
--  "%:%:"          { punctuator undefined }  

  
}




-- keywords  
<0> {
  "auto"            { keyword CTokAuto }
  "_Bool"           { keyword CTokBool }
  "break"           { keyword CTokBreak }
  "case"            { keyword CTokCase }
  "char"            { keyword CTokChar }
  "_Complex"        { keyword CTokComplex }
  "const"           { keyword CTokConst }
  "continue"        { keyword CTokContinue }
  "default"         { keyword CTokDefault }
  "do"              { keyword CTokDo }
  "double"          { keyword CTokDouble }
  "else"            { keyword CTokElse }
  "enum"            { keyword CTokEnum }
  "extern"          { keyword CTokExtern }
  "float"           { keyword CTokFloat }
  "for"             { keyword CTokFor }
  "goto"            { keyword CTokGoto }
  "if"              { keyword CTokIf }
--  "_Imaginary"      { keyword undefined }
  "inline"          { keyword CTokInline }
  "int"             { keyword CTokInt }
  "long"            { keyword CTokLong }
  "register"        { keyword CTokRegister }
  "restrict"        { keyword CTokRestrict }
  "return"          { keyword CTokReturn }
  "short"           { keyword CTokShort }
  "signed"          { keyword CTokSigned }
  "sizeof"          { keyword CTokSizeof }
  "static"          { keyword CTokStatic }
  "struct"          { keyword CTokStruct }
  "switch"          { keyword CTokSwitch }
  "typedef"         { keyword CTokTypedef }
  "union"           { keyword CTokUnion }
  "unsigned"        { keyword CTokUnsigned }
  "void"            { keyword CTokVoid }
  "volatile"        { keyword CTokVolatile }
  "while"           { keyword CTokWhile }
  
}  
     
-- predefined
<0> {
  "__func__"        { identifier }
}  

-- identifiers
<0> {
  $identifier_nondigit $identifier*     { identifier }
}

-- integer constants
<0> {
  
  $nonzero_digit $digit* @integer_suffix?       { intLiteral }
  
  0 $octal_digit* @integer_suffix?              { error "octal constant" }
  
  @hex_prefix $hex_digit @integer_suffix?       { error "hex constant" }

} 

-- char constants
<0> {
  
  \' @c_char_sequence \'        { charLiteral }

  L \' @c_char_sequence \'      { charLiteral }
}

-- string constants
<0> {

  \" @s_char_sequence? \"        { stringLiteral }

  L \" @s_char_sequence? \"      { stringLiteral }

}

{

get :: (Monad m) => Lex m CToken

alexEOF :: CToken
alexEOF = CTokEof

punctuator :: CToken -> AlexInput -> Int -> CToken
punctuator tok (p,_,_) len = tok

identifier :: AlexInput -> Int -> CToken
identifier (p,_,str) len = CTokIdent ident
  where ident = take len str

keyword :: CToken -> AlexInput -> Int -> CToken
keyword tok (p,_,_) len = tok

intLiteral :: AlexInput -> Int -> CToken
intLiteral (p,_,str) len = CTokILit lit
  where lit = read $ take len str 

--DODGY
charLiteral :: AlexInput -> Int -> CToken
charLiteral (p,_,str) len = CTokCLit lit
  where lit = read $ take len str 

stringLiteral :: AlexInput -> Int -> CToken
stringLiteral (p,_,str) len = CTokSLit lit
  where lit = take len str 





}

  