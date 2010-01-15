{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Text.ParserCombinators.ParsecExtras.HaskellLexer.Char
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- 
-- 
--
--------------------------------------------------------------------------------


module Text.ParserCombinators.ParsecExtras.HaskellLexer.Char
  (
    reservedid_h98
  , reservedop_h98

  , literal

  , dashes

  , integer
  , float

  ) where

import Text.ParserCombinators.ParsecExtras.HaskellLexer.Tokens

import Text.ParserCombinators.Parsec

import Control.Monad
import Data.Char
import Prelude hiding ( exponent )


--------------------------------------------------------------------------------
-- token sets

reservedid_h98  :: [String]
reservedid_h98  = [ "case",         "class",        "data"
                  , "default",      "deriving",     "do"
                  , "else",         "if",           "import"
                  , "in" ,          "infix",        "infixl"
                  , "infixr",       "instance",     "let"
                  , "module",       "newtype",      "of"
                  , "then",         "type",         "where"
                  , "_"
                  ]

reservedop_h98  ::  [String]
reservedop_h98  = [ "..", ":", "::", "=", "\\"
                  , "|", "<-", "->", "@", "~", "=>" ]

special_char    :: [Char]
special_char    = [ '(', ')', ',', ';', '[', ']', '`', '{', '}' ]


--------------------------------------------------------------------------------
-- tokenizers


literal         ::  CharParser st Token
literal         =  integer              ===> IntLit
               <|> float                ===> FloatLit 

newline         :: CharParser st String
newline         = schar '\n'

space           :: CharParser st String
space           = schar ' '


dashes          :: CharParser st String
dashes          = string "--" & many (char '-')

opencom         :: CharParser st String
opencom         = string "{-"

closecom        :: CharParser st String
closecom        = string "-}"


graphic         :: CharParser st Char
graphic         = small <|> large <|> symbol


small           :: CharParser st Char
small           = ascSmall <|> uniSmall <|> char '_'

ascSmall        :: CharParser st Char
ascSmall        = oneOf ['a'..'z']

uniSmall        :: CharParser st Char
uniSmall        = satisfy isLower

large           :: CharParser st Char
large           = ascLarge <|> uniLarge   -- TODO diffset

ascLarge        :: CharParser st Char
ascLarge        = oneOf ['A'..'Z']

uniLarge        :: CharParser st Char
uniLarge        = satisfy isUpper


symbol          :: CharParser st Char
symbol          = ascSymbol <|> uniSymbol_diff 

ascSymbol       :: CharParser st Char
ascSymbol       = oneOf [ '!',  '#',  '$',  '%', '&'
                        , '*',  '+',  '.',  '/', '<'
                        , '=',  '>',  '?',  '@', '\\' 
                        , '^',  '|',  '-',  '~'
                        ]

uniSymbol_diff  :: CharParser st Char
uniSymbol_diff  = satisfy $ \ch -> 
    isSymbol ch && (ch `nelem` special_char) && (ch `nelem` "_:\"'")

reservedid      :: CharParser st String
reservedid      = choose reservedid_h98

reservedop      :: CharParser st String
reservedop      = choose reservedop_h98


decimal         :: CharParser st String
decimal         = many1 digit

octal           :: CharParser st String
octal           = many1 octDigit

hexadecimal     :: CharParser st String
hexadecimal     = many1 hexDigit



integer         :: CharParser st String
integer         =  decimal
               <|> choose ["0o", "0O"] & octal
               <|> choose ["0x", "0X"] & hexadecimal


float           :: CharParser st String
float           =  decimal & schar '.' & decimal &? exponent
               <|> decimal & exponent

exponent        :: CharParser st String
exponent        = oneOf "eE" &: zeroOrOne "-+" & decimal

--------------------------------------------------------------------------------

choose :: [String] -> CharParser st String
choose = choice . map string

zeroOrOne :: [Char] -> CharParser st String
zeroOrOne cs = liftM return (oneOf cs) <|> return ""

schar :: Char -> CharParser st String
schar = liftM return . char 

soneOf :: [Char] -> CharParser st String
soneOf = liftM return  . oneOf

infixr 5 ===>

(===>) :: CharParser st String -> Lexeme -> CharParser st Token
p ===> l = getPosition >>= \pos -> p >>= \str -> return (pos,l,str)


(&) :: CharParser st String -> CharParser st String -> CharParser st String
p & q = liftM2 (++) p q


(&:) :: CharParser st Char -> CharParser st String -> CharParser st String
p &: q = liftM2 (:) p q


(&?) :: CharParser st String -> CharParser st String -> CharParser st String
p &? q = p >>= \s -> liftM (mbConcat s) (optionMaybe q)
  where 
    mbConcat s Nothing  = s
    mbConcat s (Just t) = s++t




nelem :: Eq a => a -> [a] -> Bool
nelem x xs = not (x `elem` xs)