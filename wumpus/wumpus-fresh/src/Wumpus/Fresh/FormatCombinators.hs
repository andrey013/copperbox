{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Fresh.FormatCombinators
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Fresh formatting.
--
--------------------------------------------------------------------------------

module Wumpus.Fresh.FormatCombinators
  (
    Doc
  , Format(..)
  , empty
  , showsDoc
  , (<>)
  , (<+>)  
  , hcat
  , hsep
  , vcat

  , text
  , char
  , int
  , integer
  , integral
  , float
  , double

  , space
  , comma
  , semicolon
  , line

  , fill

  , punctuate
  , enclose
  , squotes
  , dquotes
  , parens
  , brackets
  , braces
  , angles

  , lparen
  , rparen
  , lbracket
  , rbracket
  , lbrace
  , rbrace
  , langle
  , rangle

  , list
  , tupled
  , semiBraces

  , indent
  , indentLines
  , hangLines
    
  ) where

import Data.Monoid
import Data.List ( foldl' )
import Numeric

data Doc = Doc { _indent_level :: !Int, _doc_shows :: ShowS }

unDoc :: Doc -> ShowS
unDoc (Doc i sf) = if (i < 1) then sf else showString (replicate i ' ') . sf

runDoc :: Doc -> String
runDoc = ($ "") . unDoc


instance Show Doc where
  show = runDoc

instance Monoid Doc where
  mempty = empty
  mappend = (<>)


class Format a where format :: a -> Doc

--------------------------------------------------------------------------------
        
infixr 6 <>, <+>



-- | Create an empty, zero length document.
--
empty :: Doc
empty = Doc 0 id

-- | Create a document from a ShowS function.
--
showsDoc :: ShowS -> Doc
showsDoc = Doc 0 


-- | Horizontally concatenate two documents with no space 
-- between them.
-- 
(<>) :: Doc -> Doc -> Doc
Doc i a <> doc = Doc i $ a . unDoc doc 


-- | Horizontally concatenate two documents with a single space 
-- between them.
-- 
(<+>) :: Doc -> Doc -> Doc
Doc i a <+> doc = Doc i $ a . showChar ' ' . unDoc doc

-- | Horizontally concatenate a list of documents with @(\<\>)@.
--
hcat :: [Doc] -> Doc
hcat = foldr (<>) empty

-- | Horizontally concatenate a list of documents with @(\<+\>)@.
--
hsep :: [Doc] -> Doc
hsep []     = empty 
hsep [a]    = a
hsep (a:as) = foldl' (<+>) a as

-- | Vertically concatenate a list of documents, one doc per 
-- line.
--
-- Note - the tail of the list of docs is /rendered/, so applying
-- indent to the result of 'vcat' will only indent the first line.
-- 
vcat :: [Doc] -> Doc
vcat []            = Doc 0 id
vcat [a]           = a
vcat (Doc i sf:as) = Doc i (sf . foldl' fn id as)
  where
    fn acc e = acc . showChar '\n' . unDoc e


-- | Create a document from a literal string.
-- 
-- The string should not contain tabs or newlines (though this
-- is not enforced). 
--
text :: String -> Doc
text = Doc 0 . showString


-- | Create a document from a literal character.
--
-- The char should not be a tab or newline. 
--
char :: Char -> Doc
char = Doc 0 . showChar

-- | Show the Int as a Doc.
--
-- > int  = text . show
--
int :: Int -> Doc
int  = Doc 0 . showInt

-- | Show the Integer as a Doc.
--
integer :: Integer -> Doc
integer = Doc 0 . showInt

-- | Show an \"integral value\" as a Doc via 'fromIntegral'.
--
integral :: Integral a => a -> Doc
integral = Doc 0 . showInt

-- | Show the Float as a Doc.
--
float :: Double -> Doc
float = Doc 0 . showFloat

-- | Show the Double as a Doc.
--
double :: Double -> Doc
double = Doc 0 . showFloat
 
-- | Create a Doc containing a single space character.
--
space :: Doc
space = char ' '

-- | Create a Doc containing a comma, \",\".
--
comma :: Doc
comma = char ','

-- | Create a Doc containing a semi colon, \";\".
--
semicolon :: Doc
semicolon = char ';'

-- | Create a Doc containing newline, \"\\n\".
--
line :: Doc 
line = char '\n'


--------------------------------------------------------------------------------

-- | Fill a doc to the supplied length, padding the right-hand
-- side with spaces.
--
-- Note - this function is expensive - it unrolls the functional
-- representation of the String. 
-- 
-- Also it should only be used for single line Doc\'s.
-- 
fill :: Int -> Doc -> Doc
fill i d = Doc 0 (padr i ' ' $ unDoc d) 

padr :: Int -> Char -> ShowS -> ShowS
padr i c df = step (length $ df []) 
  where
    step len | len >= i  = df
             | otherwise = df . showString (replicate (i-len) c)

--------------------------------------------------------------------------------

-- | Punctuate the Doc list with the separator, producing a Doc. 
--
punctuate :: Doc -> [Doc] -> Doc
punctuate _ []     = empty
punctuate _ [x]    = x
punctuate s (x:xs) = x <> s <> punctuate s xs


-- | Enclose the final Doc within the first two.
--
-- There are no spaces between the documents:
--
-- > enclose l r d = l <> d <> r
--
enclose :: Doc -> Doc -> Doc -> Doc
enclose l r d = l <> d <> r



-- | Enclose the Doc within single quotes.
--
squotes :: Doc -> Doc
squotes = enclose (char '\'') (char '\'')

-- | Enclose the Doc within double quotes.
--
dquotes :: Doc -> Doc
dquotes = enclose (char '"') (char '"')

-- | Enclose the Doc within parens @()@.
--
parens :: Doc -> Doc
parens = enclose lparen rparen

-- | Enclose the Doc within square brackets @[]@.
--
brackets :: Doc -> Doc
brackets = enclose lbracket rbracket

-- | Enclose the Doc within curly braces @{}@.
--
braces :: Doc -> Doc
braces = enclose lbrace rbrace

-- | Enclose the Doc within angle brackets @\<\>@.
--
angles :: Doc -> Doc
angles = enclose langle rangle



-- | Create a Doc containing a left paren, \'(\'.
--
lparen :: Doc
lparen = char '('

-- | Create a Doc containing a right paren, \')\'.
--
rparen :: Doc
rparen = char ')'

-- | Create a Doc containing a left square bracket, \'[\'.
--
lbracket :: Doc
lbracket = char '['

-- | Create a Doc containing a right square bracket, \']\'.
--
rbracket :: Doc
rbracket = char ']'

-- | Create a Doc containing a left curly brace, \'{\'.
--
lbrace :: Doc
lbrace = char '{'

-- | Create a Doc containing a right curly brace, \'}\'.
--
rbrace :: Doc
rbrace = char '}'

-- | Create a Doc containing a left angle bracket, \'\<\'.
--
langle :: Doc
langle = char '<'

-- | Create a Doc containing a right angle bracket, \'\>\'.
--
rangle :: Doc
rangle = char '>'

-- | Comma separate the list of documents and enclose in square
-- brackets.
--
list :: [Doc] -> Doc
list = brackets . punctuate comma

-- | Comma separate the list of documents and enclose in parens.
--
tupled :: [Doc] -> Doc
tupled = parens . punctuate comma

-- | Separate the list with a semicolon and enclose in curly 
-- braces.
--
semiBraces :: [Doc] -> Doc
semiBraces = braces . punctuate semicolon



-- | Indent a Doc
--
indent :: Int -> Doc -> Doc
indent i (Doc n sf)  = Doc (n+i) sf


-- | Indent a list of Docs.
--
-- Note - the tail of the list of docs is /rendered/ to the 
-- supplied indent level, so identLines cannot be applied twice. 
--
-- > indentLines 2 [indentLines 2 [text "Aaa", text "Bbb", text "Ccc"]] 
--
-- Gives (with dot representing a space):
--
-- > ....Aaa 
-- > ..Bbb
-- > ..Ccc
--
indentLines :: Int -> [Doc] -> Doc
indentLines _ []            = empty
indentLines i (Doc n sf:ds) = Doc (i+n) (sf . rest) 
  where
    rest = foldr (\e acc -> (unDoc $ indent i e) . acc) id ds


-- | Print the first line at the current indentation level, then 
-- indent all subsequent lines by the initial indentation level  
-- plus the supplied Int.
--
-- > hangLines 3 [text "Aaa", text "Bbb", text "Ccc"]
--
-- Gives (with dot representing a space):
--
-- > Aaa 
-- > ...Bbb
-- > ...Ccc
--
hangLines :: Int -> [Doc] -> Doc
hangLines _ []            = empty
hangLines i (Doc n sf:ds) = Doc n (sf . rest) 
  where
    rest = foldr (\e acc -> (unDoc $ indent (i+n) e) . acc) id ds 