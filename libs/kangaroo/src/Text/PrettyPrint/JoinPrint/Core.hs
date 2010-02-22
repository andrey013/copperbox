{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Text.PrettyPrint.JoinPrint.Core
-- Copyright   :  (c) Stephen Tetley 2009-2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Printing with /join-strings/.
--
--------------------------------------------------------------------------------

module Text.PrettyPrint.JoinPrint.Core
  ( 
    Doc
  , VDoc
  , empty
  , null
  , length

  , (<>)
  , (<+>)
  , hcat
  , hsep

  , vcat
  , vsep
  , vcons
  , vsnoc

  , text
  , char
  , int
  , integer
  , integral
  , float
  , double

  , sglspace
  , dblspace
  , comma
  , semicolon


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


  , replicateChar
  , spacer


  , padl
  , padr
  , truncl 
  , truncr
  
  , render
  , renderIO
  
  ) where


import Text.PrettyPrint.JoinPrint.JoinString ( JoinString, (++) )
import qualified Text.PrettyPrint.JoinPrint.JoinString as JS

import Data.List ( foldl' )
import Data.Monoid
import Prelude hiding ( (++), null, length )


-- | Doc is the abstract data type respresenting single line 
-- documents.
--
-- JoinPrint ditinguishes between single-line and multi-line 
-- documents. Single-line, horizontal documents support some 
-- operations not multi-line documents, e.g. padding, see 'padl' 
-- and 'padr' and truncating 'truncl' and 'truncr'.
--
newtype Doc = Doc { getDoc :: JoinString }


-- | VDoc is the abstract data type respresenting multi-line 
-- documents.
--
-- Multi-line documents have a limited set of operations 
-- (basically concatenation with or without a blank line 
-- inbetween) compared to single line docs which support e.g. 
-- padding and truncating. 
--
newtype VDoc = VDoc { getVDoc :: ShowS }

--------------------------------------------------------------------------------

instance Show Doc where
  show = render

instance Show VDoc where
  show = renderV


instance Monoid Doc where
  mempty = empty
  mappend = (<>)

instance Monoid VDoc where
  mempty                      = VDoc id
  (VDoc f) `mappend` (VDoc g) = VDoc (f . showChar '\n' . g)



--------------------------------------------------------------------------------
        
infixr 6 <>, <+>

-- | Create an empty, zero length document.
--
empty :: Doc
empty = Doc $ JS.empty

-- | Test if the doc is empty.
--
null :: Doc -> Bool
null = JS.null . getDoc 


-- | Get the length of the Doc. 
-- 
-- Length is cached in the document\'s  data type so this 
-- operation is O(1).
--
length :: Doc -> Int
length = JS.length . getDoc

-- | Horizontally concatenate two documents with no space 
-- between them.
-- 
(<>) :: Doc -> Doc -> Doc
Doc a <> Doc b = Doc $ a ++ b

-- | Horizontally concatenate two documents with a single space 
-- between them.
-- 
(<+>) :: Doc -> Doc -> Doc
Doc a <+> Doc b = Doc (a ++ JS.cons1 ' ' b)

-- | Horizontally concatenate a list of documents with @(\<\>)@.
--
hcat :: [Doc] -> Doc
hcat = foldr (<>) empty

-- | Horizontally concatenate a list of documents with @(\<+\>)@.
--
hsep :: [Doc] -> Doc
hsep = foldr (<+>) empty


-- Vertically concatenation is different to PPrint or Hughes-PJ.
-- Because the Doc type tracks (horizontal) length, vertically
-- concat cannot use the same type as there is no reasonable
-- horizontal length (max length, length of first, length of 
-- last?)
--

-- | Vertically concatenate a list of documents, one doc per 
-- line.
--
-- Note - this function produces a 'VDoc' rather than a 'Doc'.
-- 
vcat :: [Doc] -> VDoc
vcat []     = VDoc id
vcat [a]    = VDoc (renderS a)
vcat (a:as) = VDoc $ foldl' fn (renderS a) as
  where
    fn f d = f . showChar '\n' . renderS d

-- | Vertically concatenate a list of documents, one doc per 
-- line with a blank line inbetween.
--
-- Note - this function produces a 'VDoc' rather than a 'Doc'.
-- 
vsep :: [Doc] -> VDoc
vsep []     = VDoc id
vsep [a]    = VDoc (renderS a)
vsep (a:as) = VDoc $ foldl' fn (renderS a) as
  where
    fn f d = f . showString "\n\n" . renderS d

-- | Prefix the 'Doc' to the start of the 'VDoc'. 
--
vcons :: Doc -> VDoc -> VDoc
vcons d (VDoc f) = VDoc (renderS d . showChar '\n' . f)

-- | Suffix the 'VDoc' with the 'Doc'. 
--
vsnoc :: VDoc -> Doc -> VDoc
vsnoc (VDoc f) d = VDoc (f . showChar '\n' . renderS d)


-- | Create a document from a literal string.
-- 
-- The string should not contain tabs or newlines (though this
-- is not enforced). To allow padding and truncating the 
-- horizontal width of a 'Doc' is cached in the datatype, 
-- building a Doc containing tabs or newlines leads to 
-- unspecified behaviour.
--
text :: String -> Doc
text = Doc . (JS.text)

-- | Create a document from a literal character.
--
-- The char should not be a tab or newline. See 'text' for the
-- rational.
--
char :: Char -> Doc
char = Doc . (JS.text) . return

-- | Show the Int as a Doc.
--
-- > int  = text . show
--
int :: Int -> Doc
int  = text . show

-- | Show the Integer as a Doc.
--
integer :: Integer -> Doc
integer = text . show

-- | Show an \"integral value\" as a Doc via 'fromIntegral'.
--
integral :: Integral a => a -> Doc
integral = integer . fromIntegral

-- | Show the Float as a Doc.
--
float :: Double -> Doc
float = text . show

-- | Show the Double as a Doc.
--
double :: Double -> Doc
double = text . show
 


-- | Create a Doc containing a single space character.
--
sglspace :: Doc
sglspace = char ' '

-- | Create a Doc containing a two-space characters.
--
dblspace :: Doc
dblspace = text "  "


-- | Create a Doc containing a comma, \",\".
--
comma :: Doc
comma = char ','

-- | Create a Doc containing a semi colon, \";\".
--
semicolon :: Doc
semicolon = char ';'


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


--------------------------------------------------------------------------------

-- | 'replicateChar' : @ n * ch -> Doc@
--
-- Repeat the supplied char (@ch@), @n@ times.
-- 
replicateChar :: Int -> Char -> Doc
replicateChar i = Doc . (JS.text) . replicate i

-- | Create a list of space characters of length @n@.
--
spacer :: Int -> Doc
spacer = replicateChar `flip` ' '

-- | 'padl' : @ width * ch * doc -> Doc @
--
-- Pad the supplied Doc to fit @width@ using the char @ch@.
-- Padding is performed at the left, right-justifying the Doc. 
-- 
-- If the doc is already wider than supplied width it is returned 
-- as-is (no truncation takes place).
--
padl :: Int -> Char -> Doc -> Doc
padl i c d = step (length d) where
  step dl | dl >= i   = d
          | otherwise = replicateChar (i-dl) c <> d 

-- | 'padr' : @ width * ch * doc -> Doc @
--
-- Pad the supplied Doc to fit @width@ using the char @ch@.
-- Padding is performed at the right, left-justifying the Doc. 
-- 
-- If the doc is already wider than supplied width it is returned
-- as-is (no truncation takes place).
--
padr :: Int -> Char -> Doc -> Doc
padr i c d = step (length d) where
  step dl | dl >= i   = d
          | otherwise = d <> replicateChar (i-dl) c


-- | 'truncl' : @width * doc -> Doc@
--
-- Truncate a doc to the supplied @width@. Characters are dropped
-- from the left until the document fits. If the document is 
-- shorter than the supplied width it is returned as is (no 
-- padding takes place).
--
truncl :: Int -> Doc -> Doc
truncl i d = step (length d) where
    step dl | dl > i    = Doc $ JS.dropLeft i (getDoc d)
            | otherwise = d

-- | 'truncr' : @width * doc -> Doc@
--
-- Truncate a doc to the supplied @width@. Characters are dropped
-- from the right until the document fits. If the document is 
-- shorter than the supplied width it is returned as is (no 
-- padding takes place).
--
truncr :: Int -> Doc -> Doc
truncr i d = step (length d) where
    step dl | dl > i    = Doc $ JS.dropRight i (getDoc d)
            | otherwise = d


-- | Rendering the Doc to a String. This is the same as using 'show'.
--
render :: Doc -> String
render = JS.toString . getDoc


renderS :: Doc -> ShowS
renderS = showString . render

renderV :: VDoc -> String
renderV = ($ "") . getVDoc


-- | Print the Doc.
--
-- > renderIO = putStrLn . render
-- 
renderIO :: Doc -> IO ()
renderIO = putStrLn . render

