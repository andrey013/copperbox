-- NOTE:
-- Alex appears to be order sensitive
-- Is this correct?

{

module Language.C.Frown.Lexer
  (Lexer, runLex, frown,
   shadowTypedef, 
   enterScope, leaveScope,
   getToken, getPosition,
   isTypeIdent, addTypedef) 
 where

import Language.C.Tokens
import Language.C.Syntax (Ident)

import Control.Monad
import Control.Monad.State
import Control.Monad.Error
import Data.Set  (Set)
import qualified Data.Set as Set (fromList, insert, member, delete, empty)

import Data.List (intersperse, concat)

}

%wrapper "clexer"

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

<0> {
  $white { skip }
}

-- decimal floating-point constants
<0> { 
  @digit_sequence @exponent $floating_suffix?   { floatLiteral }
  
  @dotted_digits @exponent? $floating_suffix?   { floatLiteral } 
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
  "__attribute__"   { keyword GnuCAttrTok } 
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
  "_Imaginary"      { keyword CTokImaginary }
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
  
  0 $octal_digit* @integer_suffix?              { octLiteral }
  
  @hex_prefix $hex_digit @integer_suffix?       { hexLiteral }

} 

-- char constants
<0> {
  
  \' @c_char_sequence \'        { charLiteral }

  L \' @c_char_sequence \'      { charLiteral }
}

-- string constants
-- (Assumption: the preprocessor has concatenated multi-line strings)

<0> {

  \" @s_char_sequence? \"        { stringLiteral }

  L \" @s_char_sequence? \"      { stringLiteral }

}

{

frown :: [String] -> CToken -> Lexer a
frown la tok = do 
  input <- alexGetInput
  line <- alexGetLine
  fail ("\n*** syntax error at " ++ position input ++ ":\n"
          ++ context input line    
          ++ "\n* expected: " ++ concat (intersperse ", " la)
          ++ "\n* actual: " ++ repr tok)

getToken :: Lexer CToken                                        
getToken = alexMonadScan

alexEOF :: CToken
alexEOF = CTokEof


shadowTypedef :: Ident -> Lexer ()
shadowTypedef ident = do
  tyids <- gets tyidents
  -- optimisation: mostly the ident will not be in
  -- the tyident set so do a member lookup to avoid
  --  churn induced by calling delete
  modify (\s -> s{tyidents = if ident `Set.member` tyids
                             then ident `Set.delete` tyids
                             else tyids})
  
                                                
                                                
enterScope :: Lexer ()
enterScope = do
  tyids <- gets tyidents
  ss    <- gets scopes
  modify (\s -> s{scopes=tyids:ss})


leaveScope :: Lexer ()
leaveScope = do
  ss <- gets scopes
  case ss of 
    [] -> fail "leaveScope: already in global scope"
    (tyidents:ss') -> modify (\s -> s{tyidents=tyidents, scopes=ss'})
  

punctuator :: CToken -> AlexInput -> Int -> Lexer CToken
punctuator tok (p,_,_) len = return tok

identifier :: AlexInput -> Int -> Lexer CToken
identifier (p,_,str) len = 
  let ident = take len str in 
  do { is_tydef <- isTypeIdent ident
     ; case is_tydef of 
        True -> return (CTokTyIdent ident) 
        False -> return (CTokIdent ident) }

keyword :: CToken -> AlexInput -> Int -> Lexer CToken
keyword tok (p,_,_) len = return tok

intLiteral :: AlexInput -> Int -> Lexer CToken
intLiteral (p,_,str) len = return (CTokILit lit)
  where lit = read $ take len str 

octLiteral :: AlexInput -> Int -> Lexer CToken
octLiteral (p,_,str) len = return (CTokILit lit)
  where lit = read $ take len str 

hexLiteral :: AlexInput -> Int -> Lexer CToken
hexLiteral (p,_,str) len = return (CTokILit lit)
  where lit = read $ take len str 

-- <DODGY>
charLiteral :: AlexInput -> Int -> Lexer CToken
charLiteral (p,_,str) len = return (CTokCLit lit)
  where lit = read $ take len str 
-- </DODGY> 

stringLiteral :: AlexInput -> Int -> Lexer CToken
stringLiteral (p,_,str) len = return (CTokSLit lit)
  where lit = take len str 

floatLiteral :: AlexInput -> Int -> Lexer CToken
floatLiteral (p,_,str) len = return (CTokFLit lit)
  where lit = take len str 

  
isTypeIdent :: Ident -> Lexer Bool
isTypeIdent ident = do
  tyids <- gets tyidents
  return (Set.member ident tyids)

addTypedef :: Ident -> Lexer ()
addTypedef ident = do
  tyids <- gets tyidents
  modify (\s -> s{tyidents = ident `Set.insert` tyids})

                             
getPosition :: Lexer (String, Int, Int)                              
getPosition = alexGetPosition

-- demo = runAlex "1+2" get


}

  