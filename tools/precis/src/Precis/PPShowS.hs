{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Precis.HString
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Pretty print combinators for ShowS
--
--------------------------------------------------------------------------------


module Precis.PPShowS
  (

    toString
  , putShowS
  , putShowSLine

  , punctuate
  , encloseSep
  , list 
  , tupled
  , semiBrace

  , hcat 
  , hsep
  , vsep
  , (<>)
  , (<+>)

  , sep
  , line

  , squotes
  , dquotes
  , braces
  , parens
  , angles
  , brackets

  , lparen
  , rparen
  , langle
  , rangle
  , lbrace
  , rbrace
  , lbracket
  , rbracket

  , sglquote
  , dblquote
  , semi
  , colon
  , comma
  , space
  , dot
  , equal
  , backslash
  , newline
  , bar

  , empty
  , text 
  , char
  , int

  , repeatChar
  , prefixLines

  ) where

infixr 5 `line`
infixr 6 <>,<+>

toString :: ShowS -> String
toString = ($ [])

putShowS :: ShowS -> IO ()
putShowS = putStr . toString

putShowSLine :: ShowS -> IO ()
putShowSLine = putStrLn . toString


punctuate :: ShowS -> [ShowS] -> [ShowS]
punctuate _ []      = []
punctuate _ [x]     = [x]
punctuate s (x:xs)  = (x . s) : punctuate s xs

encloseSep :: ShowS -> ShowS -> ShowS -> [ShowS] -> ShowS
encloseSep l r _ []  = l . r
encloseSep l r _ [x] = l . x . r
encloseSep l r s xs  = l . hcat (punctuate s xs) . r

list            :: [ShowS] -> ShowS
list            = encloseSep lbracket rbracket comma

tupled          :: [ShowS] -> ShowS
tupled          = encloseSep lparen   rparen   comma

semiBrace       :: [ShowS] -> ShowS
semiBrace       = encloseSep lbrace   rbrace   semi

hcat            :: [ShowS] -> ShowS
hcat            = fold (.)

hsep            :: [ShowS] -> ShowS
hsep            = fold sep

vsep            :: [ShowS] -> ShowS
vsep            = fold line


(<>)            :: ShowS -> ShowS -> ShowS
(<>)            = (.)

(<+>)           :: ShowS -> ShowS -> ShowS
(<+>) f g       = f <> space <> g


fold :: (ShowS -> ShowS -> ShowS) -> [ShowS] -> ShowS
fold _ []      = id
fold f xs      = foldr1 f xs

sep             :: ShowS -> ShowS -> ShowS
x `sep`  y      = x . space . y  

line            :: ShowS -> ShowS -> ShowS
x `line` y      = x . newline . y

squotes         :: ShowS -> ShowS
squotes         = enclose sglquote sglquote

dquotes         :: ShowS -> ShowS
dquotes         = enclose dblquote dblquote

braces          :: ShowS -> ShowS
braces          = enclose lbrace rbrace

parens          :: ShowS -> ShowS
parens          = enclose lparen rparen

angles          :: ShowS -> ShowS
angles          = enclose langle rangle

brackets        :: ShowS -> ShowS
brackets        = enclose lbracket rbracket

enclose         :: ShowS -> ShowS -> ShowS -> ShowS
enclose l r x   = l . x . r

lparen          :: ShowS
lparen          = showChar '('

rparen          :: ShowS
rparen          = showChar ')'

langle          :: ShowS
langle          = showChar '<'

rangle          :: ShowS
rangle          = showChar '>'

lbrace          :: ShowS
lbrace          = showChar '{'

rbrace          :: ShowS
rbrace          = showChar '}'

lbracket        :: ShowS
lbracket        = showChar '['

rbracket        :: ShowS
rbracket        = showChar ']'     


sglquote        :: ShowS
sglquote        = showChar '\''


dblquote        :: ShowS
dblquote        = showChar '"'

semi            :: ShowS
semi            = showChar ';'

colon           :: ShowS
colon           = showChar ':'

comma           :: ShowS
comma           = showChar ','

space           :: ShowS
space           = showChar ' '

dot             :: ShowS
dot             = showChar '.'

equal           :: ShowS
equal           = showChar '='

backslash       :: ShowS
backslash       = showChar '\\'

newline         :: ShowS
newline         = showChar '\n'

bar             :: ShowS
bar             = showChar '|'


empty           :: ShowS
empty           = id

text            :: String -> ShowS
text            = showString

char            :: Char -> ShowS
char            = showChar

int             :: Int -> ShowS
int             = shows


repeatChar      :: Int -> Char -> ShowS
repeatChar i    = showString . replicate i


prefixLines     :: ShowS -> String -> ShowS
prefixLines pre xs = vsep $ map ((pre <>) . text) $ lines xs